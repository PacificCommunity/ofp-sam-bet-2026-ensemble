"""Byte-only compact-source checks. No model, container or network execution."""
import copy
import hashlib
import json
from pathlib import Path
import shutil
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'rr-test'))
from saved import Source


class CompactSourceGuards(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = Source(ROOT)

    def test_failed_cases_have_no_restorable_PAR(self):
        for number in ('001','022','025','080'):
            with self.subTest(number=number), self.assertRaisesRegex(ValueError,'No saved final PAR'):
                self.source.native_files('rrtest-'+number+'-rr1')

    def test_actual_RR_native_restore_hashes_and_receipt(self):
        with tempfile.TemporaryDirectory() as temp:
            output=Path(temp)/'new'
            self.source.restore('rrtest-005-rr1',output)
            receipt=json.loads((output/'saved-inputs.json').read_text())
            self.assertEqual(set(receipt['files']),{'final.par','doitall.sh','bet.frq','bet.ini','bet.tag','bet.age_length','bet.reg_scaling','mfcl.cfg'})
            for name,row in receipt['files'].items():
                self.assertEqual(hashlib.sha256((output/name).read_bytes()).hexdigest(),row['sha256'])
            self.assertEqual(receipt['files']['final.par']['sha256'],self.source.model('rrtest-005-rr1')['final_par']['sha256'])

    def test_existing_or_checkout_output_refused_before_read(self):
        with tempfile.TemporaryDirectory() as temp:
            root=Path(temp)
            for kind in ('directory','file','dangling-symlink'):
                output=root/kind
                if kind=='directory':output.mkdir()
                elif kind=='file':output.write_bytes(b'foreign')
                else:output.symlink_to(root/'absent')
                with self.subTest(kind=kind),patch.object(self.source,'native_files') as read:
                    with self.assertRaisesRegex(ValueError,'Existing output'):
                        self.source.restore('rrtest-005-rr1',output)
                    read.assert_not_called()
        with patch.object(self.source,'native_files') as read:
            with self.assertRaisesRegex(ValueError,'beneath outputs'):
                self.source.restore('rrtest-005-rr1',ROOT/'forbidden-new-native-case')
            read.assert_not_called()

    def test_archive_bitflip_refused_at_outer_hash(self):
        with tempfile.TemporaryDirectory() as temp:
            root=Path(temp)
            shutil.copytree(ROOT/'reproduce',root/'reproduce')
            (root/'rr-test').mkdir()
            shutil.copyfile(ROOT/'rr-test/native-models.json',root/'rr-test/native-models.json')
            archive=root/'reproduce/native.tar.gz'
            data=bytearray(archive.read_bytes());data[-1]^=1;archive.write_bytes(data)
            with self.assertRaisesRegex(ValueError,'Saved bytes differ'):
                Source(root)

    def test_changed_PAR_and_central_bytes_refused(self):
        for key in ('final_par','reference_rep'):
            with self.subTest(key=key):
                source=copy.copy(self.source);source.payload=dict(self.source.payload)
                record=source.model('rrtest-005-rr1')[key]
                source.payload[record['member']]=b'changed saved bytes\n'
                with self.assertRaisesRegex(ValueError,'Saved bytes differ'):
                    source.read(record)

    def test_replacing_an_unpaired_original_model_is_refused(self):
        with tempfile.TemporaryDirectory() as temp:
            root=Path(temp)
            shutil.copytree(ROOT/'reproduce',root/'reproduce')
            for name in ('data/ensemble/retained-final-par-manifest.csv','rr-test/results/run-manifest.csv'):
                target=root/name;target.parent.mkdir(parents=True,exist_ok=True);shutil.copyfile(ROOT/name,target)
            index=json.loads((ROOT/'rr-test/native-models.json').read_text())
            for row in index['models']:
                if row['model']=='ensemble-002':row['model']='ensemble-999'
            closure=json.loads((root/'reproduce/closure.json').read_text())
            for row in closure['models']:
                if row['model']=='ensemble-002':row['model']='ensemble-999'
            closure_bytes=(json.dumps(closure,indent=2)+'\n').encode()
            (root/'reproduce/closure.json').write_bytes(closure_bytes)
            recipe=json.loads((root/'reproduce/package.json').read_text())
            recipe['closure']['sha256']=hashlib.sha256(closure_bytes).hexdigest()
            recipe_bytes=(json.dumps(recipe,indent=2)+'\n').encode()
            (root/'reproduce/package.json').write_bytes(recipe_bytes)
            index['package_sha256']=hashlib.sha256(recipe_bytes).hexdigest()
            (root/'rr-test/native-models.json').write_text(json.dumps(index))
            with self.assertRaisesRegex(ValueError,'membership differs'):
                Source(root)

    def test_member_mode_cannot_override_checked_manifest(self):
        record=dict(self.source.model('rrtest-005-rr1')['final_par'],mode=0o4755)
        with self.assertRaisesRegex(ValueError,'member binding'):
            self.source.read(record)

    def test_original_input_binding_cannot_be_changed_to_other_hash(self):
        source=copy.copy(self.source);source.models=copy.deepcopy(self.source.models)
        source.model('rrtest-005-rr1')['native_input_sha256']['bet.ini']='0'*64
        with self.assertRaisesRegex(ValueError,'original hashes'):
            source.input_hashes('rrtest-005-rr1')

    def test_changed_original_input_manifest_refused(self):
        source=copy.copy(self.source)
        with tempfile.TemporaryDirectory() as temp:
            source.root=Path(temp)
            binding=source.model('rrtest-005-rr1')['input_manifest']
            file=source.root/binding['path'];file.parent.mkdir(parents=True)
            file.write_bytes(b'changed input binding\n')
            with self.assertRaisesRegex(ValueError,'manifest checksum'):
                source.input_hashes('rrtest-005-rr1')

    def test_unknown_storage_is_not_a_repository_fallback(self):
        with self.assertRaisesRegex(ValueError,'Unknown saved-file storage'):
            self.source.read({'storage':'unknown'})


if __name__=='__main__':unittest.main()
