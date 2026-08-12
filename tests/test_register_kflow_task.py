#!/usr/bin/env python3
"""Unit checks for the dual-site campaign's safety-critical pure functions."""

from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    "register_kflow_task", ROOT / "scripts" / "register-kflow-task.py"
)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError("Could not load scripts/register-kflow-task.py")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class CampaignTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.config, cls.rows, cls.sites, cls.summary = MODULE.read_inputs()
        cls.commit = MODULE.git("rev-parse", "HEAD")
        cls.task = MODULE.task_payload(cls.config, cls.rows, cls.sites, cls.commit)
        cls.jobs = [
            MODULE.job_payload(cls.config, row, site, index, cls.commit)
            for index, (row, site) in enumerate(zip(cls.rows, cls.sites), 1)
        ]

    def test_frozen_site_assignment_contract(self) -> None:
        self.assertEqual(self.summary["counts"], {"noumea": 50, "suva": 50})
        self.assertAlmostEqual(
            self.summary["max_abs_axis_spearman"], 0.02461829819586655, places=15
        )
        self.assertEqual(
            [row["submission_site"] for row in self.sites],
            MODULE.generated_site_vector(),
        )

    def test_all_payloads_are_independent_and_site_pinned(self) -> None:
        self.assertEqual(len(self.jobs), 100)
        self.assertEqual(len({job["env"]["JOB_KEY"] for job in self.jobs}), 100)
        counts = {site: 0 for site in MODULE.SITE_CONTRACT}
        for job in self.jobs:
            site = job["metadata"]["submission_site"]
            counts[site] += 1
            contract = MODULE.SITE_CONTRACT[site]
            self.assertEqual(job["remote_host"], contract["remote_host"])
            self.assertEqual(job["slot_requirements"], contract["slot_requirements"])
            self.assertEqual(job["input_jobs"], [])
            self.assertEqual(job["triggers"], {})
            self.assertNotIn("artifacts", job)
            self.assertNotIn("attachments", job)
            self.assertNotIn("SUBMISSION_SITE", job["env"])
            self.assertEqual(job["env"]["EXPECTED_PROGRAM_SHA256"], MODULE.EXPECTED_PROGRAM_SHA256)
            self.assertEqual(job["env"]["EXPECTED_REPOSITORY_COMMIT"], self.commit)
        self.assertEqual(counts, {"noumea": 50, "suva": 50})

    def test_duplicate_job_keys_are_rejected(self) -> None:
        summaries = [
            {"job_number": 1, "env": {"JOB_KEY": "ensemble-001"}},
            {"job_number": 2, "env": {"JOB_KEY": "ensemble-001"}},
        ]
        with self.assertRaisesRegex(ValueError, "Duplicate JOB_KEY"):
            MODULE.jobs_by_key(summaries, {"ensemble-001"})

    def test_unexpected_job_keys_are_rejected(self) -> None:
        with self.assertRaisesRegex(ValueError, "unexpected JOB_KEY"):
            MODULE.jobs_by_key(
                [{"job_number": 1, "env": {"JOB_KEY": "wrong-row"}}],
                {"ensemble-001"},
            )

    def test_matching_existing_task_is_accepted(self) -> None:
        stored = {**self.task, "code": MODULE.TASK_NAME}
        MODULE.verify_task_contract(stored, self.task)

    def test_existing_task_drift_is_rejected(self) -> None:
        stored = {**self.task, "code": MODULE.TASK_NAME, "branch": "main"}
        with self.assertRaisesRegex(ValueError, "task branch"):
            MODULE.verify_task_contract(stored, self.task)

    def test_registered_source_commit_comes_from_consistent_task_snapshot(self) -> None:
        stored = {**self.task, "code": MODULE.TASK_NAME}
        self.assertEqual(MODULE.registered_source_commit(stored), self.commit)

    def test_registered_source_commit_rejects_incomplete_or_disagreeing_snapshot(self) -> None:
        missing = {**self.task, "code": MODULE.TASK_NAME, "metadata": {**self.task["metadata"]}}
        del missing["metadata"]["source_commit"]
        with self.assertRaisesRegex(ValueError, "complete source-commit provenance"):
            MODULE.registered_source_commit(missing)

        disagreeing = {
            **self.task,
            "code": MODULE.TASK_NAME,
            "metadata": {**self.task["metadata"], "source_commit": "0" * 40},
        }
        with self.assertRaisesRegex(ValueError, "source-commit provenance disagrees"):
            MODULE.registered_source_commit(disagreeing)

    def test_registered_source_commit_rejects_invalid_sha(self) -> None:
        invalid = {
            **self.task,
            "code": MODULE.TASK_NAME,
            "env": {**self.task["env"], "EXPECTED_REPOSITORY_COMMIT": "not-a-sha"},
        }
        with self.assertRaisesRegex(ValueError, "invalid source-commit provenance"):
            MODULE.registered_source_commit(invalid)

    def test_existing_task_is_checked_before_any_registration_post(self) -> None:
        stored = {**self.task, "code": MODULE.TASK_NAME}
        with mock.patch.object(MODULE, "get_report", return_value=stored), mock.patch.object(
            MODULE, "api_json", side_effect=AssertionError("POST must not be attempted")
        ) as api:
            report, created = MODULE.register_or_verify_task("http://kflow", "token", self.task)
        self.assertIs(report, stored)
        self.assertFalse(created)
        api.assert_not_called()

    def test_job_listing_reads_every_page(self) -> None:
        responses = [
            {"jobs": [{"job_number": 2, "env": {"JOB_KEY": "ensemble-002"}}]},
            {"jobs": [{"job_number": 1, "env": {"JOB_KEY": "ensemble-001"}}]},
            {"jobs": []},
        ]
        with mock.patch.object(MODULE, "api_json", side_effect=responses) as api:
            jobs = MODULE.list_all_jobs("http://kflow", "token")
        self.assertEqual(len(jobs), 2)
        self.assertEqual(api.call_count, 3)
        self.assertIn("page=3", api.call_args_list[-1].args[1])

    def test_ambiguous_post_is_reconciled_without_retry(self) -> None:
        expected = self.jobs[0]
        key = expected["env"]["JOB_KEY"]
        summary = {"job_number": 1, "env": {"JOB_KEY": key}}
        with mock.patch.object(MODULE, "api_json", side_effect=TimeoutError("timed out")) as api, mock.patch.object(
            MODULE, "list_all_jobs", return_value=[summary]
        ), mock.patch.object(MODULE, "wait_for_resolved_checkouts") as settled, mock.patch.object(MODULE.time, "sleep"):
            reconciled, submitted = MODULE.submit_missing_jobs(
                "http://kflow", "token", {key: expected}, {}, 1, 2, 2
            )
        self.assertEqual(set(reconciled), {key})
        self.assertEqual(submitted, [key])
        self.assertEqual(api.call_count, 1)
        settled.assert_called_once()

    def persisted_job(self, expected):
        return {
            "report_code": MODULE.TASK_NAME,
            "repo_full_name": expected["repo"],
            "branch": expected["branch"],
            "command": expected["command"],
            "docker_image": expected["docker_image"],
            "remote_host": expected["remote_host"],
            "remote_user": expected["remote_user"],
            "cpus": expected["cpus"],
            "memory": expected["memory"],
            "disk": expected["disk"],
            "batch_name": expected["batch_name"],
            "remote_dir": expected["remote_base_dir"] + "/test/job",
            "env": expected["env"],
            "tags": expected["tags"],
            "metadata": expected["metadata"],
            "details": {
                "git_commit_sha": self.commit,
                "report_spec": {
                    "slot_requirements": expected["slot_requirements"],
                    "input_jobs": [],
                    "resolved_input_jobs": [],
                    "input_files": [],
                    "triggers": {},
                    "artifacts": [],
                    "output_patterns": expected["output_patterns"],
                    "checkout": expected["checkout"],
                    "exclude_machines": [],
                    "exclude_slots": [],
                },
            },
        }

    def test_resolved_checkout_commit_is_required(self) -> None:
        expected = self.jobs[0]
        stored = self.persisted_job(expected)
        MODULE.verify_job_contract(stored, expected)
        stored["details"]["git_commit_sha"] = "0" * 40
        with self.assertRaisesRegex(ValueError, "resolved git_commit_sha"):
            MODULE.verify_job_contract(stored, expected)

    def test_noumea_local_host_normalization_is_accepted_with_matching_slot(self) -> None:
        expected = next(
            job for job in self.jobs if job["metadata"]["submission_site"] == "noumea"
        )
        stored = self.persisted_job(expected)
        stored["remote_host"] = "local"
        stored["remote_host_slot"] = "slot1_1@nouofpcand03.corp.spc.int"
        MODULE.verify_job_contract(stored, expected)

    def test_local_host_normalization_is_rejected_for_suva(self) -> None:
        expected = next(
            job for job in self.jobs if job["metadata"]["submission_site"] == "suva"
        )
        stored = self.persisted_job(expected)
        stored["remote_host"] = "local"
        stored["remote_host_slot"] = "slot1_1@suvofpcand03.corp.spc.int"
        with self.assertRaisesRegex(ValueError, "remote_host"):
            MODULE.verify_job_contract(stored, expected)

    def test_remote_host_slot_must_match_expected_site_when_present(self) -> None:
        expected = next(
            job for job in self.jobs if job["metadata"]["submission_site"] == "noumea"
        )
        stored = self.persisted_job(expected)
        stored["remote_host"] = "local"
        stored["remote_host_slot"] = "slot1_1@suvofpcand03.corp.spc.int"
        with self.assertRaisesRegex(ValueError, "remote_host_slot"):
            MODULE.verify_job_contract(stored, expected)

    def test_canonical_remote_host_still_checks_persisted_slot_prefix(self) -> None:
        expected = next(
            job for job in self.jobs if job["metadata"]["submission_site"] == "suva"
        )
        stored = self.persisted_job(expected)
        stored["remote_host_slot"] = "slot1_1@nouofpcand03.corp.spc.int"
        with self.assertRaisesRegex(ValueError, "remote_host_slot"):
            MODULE.verify_job_contract(stored, expected)


if __name__ == "__main__":
    unittest.main()
