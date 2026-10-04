#!/usr/bin/env python3
"""Check the frozen RR kit in GitHub CI; each native call uses an isolated network."""
import argparse
import hashlib
import importlib.util
import io
import json
import math
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import sys
import tempfile
import zipfile

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
ZIP_SHA = "4b809f2dfa5de90902254f0802e0e67ff4e607398d4154643ebf6c6f0805bb27"
ZIP_BYTES = 20941714
ENGINE_SHA = "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"
PAIR_NUMBERS = (5,6,7,8,14,27,29,32,33,34,35,40,42,43,44,45,46,49,52,62,63,65,71,74,78,86,89,92,94,95)
MODELS = tuple(sorted([f"ensemble-{n:03d}" for n in PAIR_NUMBERS]
                      + [f"rrtest-{n:03d}-rr1" for n in PAIR_NUMBERS]))
CONTROLS = ["1 1 1","1 50 -4","1 121 0","1 186 0","1 187 0","1 188 0","1 189 0","1 190 1","1 246 1"]
MEMBERS = {"CONTENTS.sha256","README.md","manifest.json","mfclo64","native.tar.gz",
           "reproduce/native_directory.py","rr-test/evaluate-final.py",
           "rr-test/saved.py","run-final"}


def require(test, message):
    if not test:
        raise ValueError(message)


def digest(data):
    return hashlib.sha256(data).hexdigest()


def read_regular(path, bound=40_000_000):
    fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
    try:
        before = os.fstat(fd)
        require(stat.S_ISREG(before.st_mode) and before.st_nlink == 1 and before.st_size <= bound,
                "Expected a bounded regular file: " + str(path))
        data, offset = [], 0
        while True:
            chunk = os.pread(fd, 1024*1024, offset)
            if not chunk: break
            data.append(chunk);offset += len(chunk)
        after, named = os.fstat(fd), os.stat(path, follow_symlinks=False)
        signature = lambda x:(x.st_dev,x.st_ino,x.st_size,x.st_mtime_ns,x.st_ctime_ns)
        require(signature(before)==signature(after)==signature(named), "File changed while reading")
        return b"".join(data)
    finally:
        os.close(fd)


def verify_zip(path):
    data = read_regular(path)
    require(len(data)==ZIP_BYTES and digest(data)==ZIP_SHA, "Frozen standalone ZIP differs")
    return data


def extract_kit(data, destination):
    seen = set()
    destination.mkdir(mode=0o700)
    with zipfile.ZipFile(io.BytesIO(data)) as archive:
        for member in archive.infolist():
            prefix = "bet-2026-rr-standalone/"
            require(member.filename.startswith(prefix), "Unexpected ZIP root")
            name = member.filename[len(prefix):]
            require(name in MEMBERS and name not in seen and not member.is_dir()
                    and member.create_system==3 and stat.S_ISREG(member.external_attr >> 16),
                    "Unexpected, duplicate or nonregular ZIP member")
            mode = 0o755 if name in ("run-final","mfclo64") else 0o644
            require(stat.S_IMODE(member.external_attr >> 16)==mode, "ZIP member mode differs")
            require(member.file_size<=40_000_000, "ZIP member exceeds size bound")
            path = destination/name;path.parent.mkdir(parents=True,exist_ok=True)
            with path.open("xb") as output: output.write(archive.read(member))
            path.chmod(mode);seen.add(name)
    require(seen==MEMBERS, "Incomplete ZIP inventory")
    sums = {}
    for line in read_regular(destination/"CONTENTS.sha256").decode().splitlines():
        match = re.fullmatch(r"([0-9a-f]{64})  (.+)",line)
        require(match is not None, "Invalid CONTENTS line")
        sha,name = match.groups()
        require(name in MEMBERS-{"CONTENTS.sha256"} and name not in sums, "Invalid CONTENTS member")
        require(digest(read_regular(destination/name))==sha, "Extracted CONTENTS hash differs")
        sums[name]=sha
    require(sums.keys()==MEMBERS-{"CONTENTS.sha256"}, "Incomplete CONTENTS inventory")
    return sums


def parse_native_log(data):
    text = data.decode("utf-8", errors="strict")
    limits, counters = [], []
    for line in text.splitlines():
        control = re.fullmatch(r"\s*optfile\.cpp\s+(.+?)\s*",line)
        if control:
            values = control.group(1).split()
            require(len(values)>=3 and all(re.fullmatch(r"[-+]?\d+",x) for x in values[:3]),
                    "Malformed native optfile control")
            first = tuple(int(x) for x in values[:3])
            if first[:2]==(1,1):
                require(first==(1,1,1), "Native function-evaluation ceiling was not one")
                limits.append(line.strip())
        if "variables;" in line and re.search(r"function\s+evaluation",line):
            counter = re.fullmatch(r"\s*(\d+)\s+variables;\s+iteration\s+(\d+);\s+function\s+evaluation\s+(\d+)\s*",line)
            require(counter is not None, "Malformed native iteration/evaluation counter")
            require(tuple(map(int,counter.groups()))==(1997,0,0), "Native counter differs from 1997/0/0")
            counters.append(line.strip())
    require(limits and counters, "Native ceiling-one control or actual zero counter evidence is absent")
    objectives = re.findall(r"^\s*Total func\s+(\S+)\s*$",text,re.MULTILINE)
    require(objectives, "Native Total func is absent")
    objective = float(objectives[0]);require(math.isfinite(objective), "Nonfinite native objective")
    return {"native_log_sha256":digest(data),"native_control_records":len(limits),"native_counter_records":len(counters),
            "native_control_snippets":limits[:20],"native_counter_snippets":counters[:20],
            "all_native_counters_1997_0_0":True,"first_logged_objective":objective,
            "function_evaluation_ceiling":1,"reported_iteration":0,"reported_function_counter":0}


def check_receipt(receipt, row, expected, evaluated, logged):
    require(receipt["mode"]=="outputs-only" and type(receipt["function_evaluation_limit"]) is int
            and receipt["function_evaluation_limit"]==1 and receipt["controls"]==CONTROLS,
            "Receipt does not describe the exact original ceiling-one controls")
    require(receipt["mfcl_sha256"]==ENGINE_SHA and receipt["native_exit_code"] in (0,3)
            and receipt["input_unchanged"] is True, "Native engine/exit/input receipt differs")
    require(receipt["input_par_sha256"]==row["final_par"]["sha256"]
            and receipt["native_input_sha256"]==row["native_input_sha256"]
            and receipt["reference_rep_sha256"]==row["reference_rep"]["sha256"], "Saved model binding differs")
    require(receipt["parameters"]==1997, "Parameter count differs")
    values = [expected,evaluated,logged,receipt["saved_objective"],receipt["evaluated_objective"],receipt["logged_objective"]]
    require(all(type(x) in (int,float) and math.isfinite(x) for x in values), "Nonfinite objective")
    require(all(abs(x-expected)<=1e-6 for x in values), "Objective differs from saved PAR")


def shard_models(shard):
    require(type(shard) is int and 0<=shard<3, "Choose shard 0, 1 or 2")
    result = MODELS[shard*20:(shard+1)*20]
    require(len(result)==20, "Shard must contain exactly 20 cases")
    return result


def save_json(path, value):
    with path.open("x") as output:
        json.dump(value,output,indent=2,sort_keys=True,allow_nan=False);output.write("\n")


def command(kit, model, output, prepare=False):
    base = [sys.executable,str(kit/"run-final")]
    if prepare: return base+["--prepare-only",model,str(output)]
    return ["sudo","-n","unshare","--net","--setgid",str(os.getgid()),"--setuid",str(os.getuid()),"--"]+base+[model,str(output)]


def check_working_copy(output, row, source):
    bindings = dict(row["files"], **{"final.par":row["final_par"]})
    bindings.update(row["refit_files"])
    for name,binding in bindings.items():
        data = read_regular(output/name)
        require(data==source.read(binding) and stat.S_IMODE((output/name).stat().st_mode)==binding["mode"],
                "Working-copy bytes or mode differ: " + name)
    require(digest(read_regular(output/"mfclo64"))==ENGINE_SHA, "Working-copy engine differs")
    require(read_regular(output/"original-INPUTS.sha256")==source.read(row["input_manifest"]), "Original input ledger differs")


def inventory_exclusions(row):
    return set(row["files"]) | set(row["refit_files"]) | {
        "final.par","input.par","mfclo64","saved-inputs.json","original-INPUTS.sha256",
        "mfcl-evaluation.log","evaluation-check.json","native-counter-check.json"}


def check_output_inventory(inventory, row, receipt):
    require(isinstance(inventory,list), "Generated output inventory must be a list")
    by_name={};excluded=inventory_exclusions(row)
    for record in inventory:
        require(isinstance(record,dict) and set(record)=={"path","bytes","sha256"}, "Malformed generated output record")
        name=record["path"]
        require(isinstance(name,str) and name and not name.startswith("/") and "\\" not in name
                and all(part not in ("",".","..") for part in name.split("/"))
                and not any(ord(character)<32 for character in name), "Unsafe generated output path")
        require(name not in excluded and name not in by_name, "Staged or duplicate generated output path")
        require(type(record["bytes"]) is int and record["bytes"]>=0
                and isinstance(record["sha256"],str) and re.fullmatch(r"[0-9a-f]{64}",record["sha256"]) is not None,
                "Invalid generated output size or SHA256")
        by_name[name]=record
    for name in ("evaluated.par","plot-evaluated.par.rep"):
        require(name in by_name and by_name[name]["bytes"]>0
                and {key:by_name[name][key] for key in ("bytes","sha256")}==receipt["files"][name],
                "Required output inventory differs from evaluated file receipt")


def generated_output_inventory(output, row, require_outputs=True):
    excluded = inventory_exclusions(row)
    result = []
    for folder, directories, files in os.walk(output, followlinks=False):
        require(all(stat.S_ISDIR((Path(folder)/name).lstat().st_mode) for name in directories),
                "Nonregular generated directory refused")
        for name in files:
            path=Path(folder)/name;relative=path.relative_to(output).as_posix()
            if relative in excluded: continue
            fd=os.open(path,os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK)
            try:
                before=os.fstat(fd)
                require(stat.S_ISREG(before.st_mode) and before.st_nlink==1, "Generated output must be regular")
                hashed=hashlib.sha256();size=0
                while True:
                    chunk=os.read(fd,1024*1024)
                    if not chunk: break
                    hashed.update(chunk);size+=len(chunk)
                signature=lambda x:(x.st_dev,x.st_ino,x.st_size,x.st_mtime_ns,x.st_ctime_ns)
                require(signature(before)==signature(os.fstat(fd))==signature(path.lstat()) and size==before.st_size,
                        "Generated output changed while hashing")
                result.append({"path":relative,"bytes":size,"sha256":hashed.hexdigest()})
            finally:os.close(fd)
    by_name={row["path"]:row for row in result}
    if require_outputs:
        require(all(name in by_name and by_name[name]["bytes"]>0 for name in ("evaluated.par","plot-evaluated.par.rep")),
                "Required nonempty generated outputs absent from inventory")
    return sorted(result,key=lambda row:row["path"])


def input_file_hashes(output, row):
    names=set(row["files"])|set(row["refit_files"])|{"final.par","input.par","mfclo64"}
    result={}
    for name in sorted(names):
        path=output/name
        if not os.path.lexists(path):
            result[name]={"present":False};continue
        data=read_regular(path)
        result[name]={"present":True,"bytes":len(data),"sha256":digest(data)}
    return result


def rep_layout(data, fields):
    sections=[];current=None
    for number,line in enumerate(data.decode("utf-8",errors="replace").splitlines(),1):
        if line.lstrip().startswith("#"):
            current={"label":line.lstrip()[1:].strip(),"header_line":number,"rows":0,"widths":set()}
            sections.append(current)
        elif line.strip() and current is not None:
            current["rows"]+=1;current["widths"].add(len(line.split()))
    for section in sections: section["widths"]=sorted(section["widths"])
    return {"sections":sections,"required_fields":{name:[section for section in sections if section["label"]==name]
                                                   for name in sorted(fields)}}


def retain_failure(proof, model, evaluated, row, source, checks, process, prepared_hashes, error):
    """Preserve diagnostics without granting a failed evaluation a pass receipt."""
    report={"schema_version":1,"status":"failed-diagnostic-only","model":model,
            "commit":os.environ.get("GITHUB_SHA","UNKNOWN"),"zip_sha256":ZIP_SHA,
            "engine_sha256":ENGINE_SHA,"controls":CONTROLS,"function_evaluation_ceiling":1,"failure":str(error),
            "wrapper_exit_code":None if process is None else process.returncode,
            "native_exit_code":None,"native_exit_status":"UNKNOWN; success receipt not observed",
            "prepared_copy_input_hashes":prepared_hashes,
            "prepared_copy_note":"Separate prepare-only directory, not the evaluated directory's pre-native snapshot",
            "evaluated_copy_before_input_hashes":None,"evaluated_copy_before_status":"UNKNOWN; no pre-native snapshot retained",
            "expected_native_input_sha256":row["native_input_sha256"],
            "expected_final_par_sha256":row["final_par"]["sha256"],"retained_files":{},"diagnostic_errors":[]}
    def attempt(label, action):
        try: action()
        except Exception as capture_error: report["diagnostic_errors"].append({"stage":label,"error":str(capture_error)})
    def preserve(name, data):
        target=proof/(model+".failure-"+name)
        with target.open("xb") as output:output.write(data)
        report["retained_files"][name]={"path":target.name,"bytes":len(data),"sha256":digest(data)}
    if process is not None:
        attempt("wrapper-output",lambda:preserve("wrapper.log",process.stdout.encode("utf-8")))
        if "invalid REP dimensions:" in process.stdout:
            report["inferred_native_allowed_exit_codes"]=[0,3]
            report["inference_basis"]="Frozen evaluator only reaches compare_rep after accepting native exit 0 or 3"
    def retain_native(name, alias):
        path=evaluated/name
        if os.path.lexists(path):preserve(alias,read_regular(path,bound=1_000_000_000 if name=="mfcl-evaluation.log" else 40_000_000))
    for name,alias in (("mfcl-evaluation.log","native.log"),("evaluated.par","evaluated.par"),
                       ("plot-evaluated.par.rep","plot-evaluated.par.rep"),("evaluation-check.json","evaluation-check.json")):
        attempt(name,lambda name=name,alias=alias:retain_native(name,alias))
    if evaluated.is_dir() and not evaluated.is_symlink():
        attempt("after-input-hashes",lambda:report.update(evaluated_copy_after_input_hashes=input_file_hashes(evaluated,row)))
        attempt("generated-output-inventory",lambda:report.update(generated_output_inventory=generated_output_inventory(evaluated,row,require_outputs=False)))
        if os.path.lexists(evaluated/"plot-evaluated.par.rep"):
            attempt("REP-layout",lambda:report.update(generated_rep_layout=rep_layout(read_regular(evaluated/"plot-evaluated.par.rep"),checks.REP_FIELDS)))
        if os.path.lexists(evaluated/"evaluation-check.json"):
            def native_status():
                receipt=json.loads(read_regular(evaluated/"evaluation-check.json"))
                require(type(receipt["native_exit_code"]) is int,"Malformed native exit receipt")
                report.update(native_exit_code=receipt["native_exit_code"],native_exit_status="Observed in frozen controller receipt; failed outer gate remains failed")
            attempt("native-exit-receipt",native_status)
    attempt("reference-REP-layout",lambda:report.update(reference_rep_layout=rep_layout(source.read(row["reference_rep"]),checks.REP_FIELDS)))
    save_json(proof/(model+".failure-diagnostic.json"),report)


def run_shard(args):
    expected = shard_models(args.shard)
    repo = ROOT.resolve()
    proof = args.proof_dir.parent.resolve()/args.proof_dir.name
    work_parent = args.work_parent.resolve()
    require(repo not in work_parent.parents and repo not in proof.parents and work_parent != repo and proof != repo,
            "CI generated directories must be outside the repository")
    require(not os.path.lexists(proof), "Existing proof directory refused")
    proof.mkdir(mode=0o700)
    root = Path(tempfile.mkdtemp(prefix=f"rr-standalone-{args.shard}-",dir=work_parent)).resolve()
    held = (root.stat().st_dev,root.stat().st_ino)
    report = {"schema_version":1,"shard":args.shard,"zip_sha256":ZIP_SHA,"engine_sha256":ENGINE_SHA,
              "commit":os.environ.get("GITHUB_SHA","UNKNOWN"),"expected_models":list(expected),
              "prepared_models":[],"passed_models":[],"status":"failed"}
    owned = {}
    def clean(path):
        require(path in owned and path.parent==root and (root.stat().st_dev,root.stat().st_ino)==held,
                "Cleanup ownership differs")
        current = path.lstat()
        require(stat.S_ISDIR(current.st_mode) and (current.st_dev,current.st_ino)==owned[path], "Generated directory replaced")
        shutil.rmtree(path);del owned[path]
    try:
        zip_path = repo/"rr-test/standalone.zip"
        inventory = extract_kit(verify_zip(zip_path),root/"kit")
        kit = root/"kit"
        subprocess.run([sys.executable,str(kit/"run-final"),"--verify"],check=True)
        listed = subprocess.check_output([sys.executable,str(kit/"run-final"),"--list"],text=True).splitlines()
        require(tuple(listed)==MODELS, "Kit selectors differ from exact 60-case membership")
        sys.path.insert(0,str(kit/"rr-test"));sys.path.insert(0,str(kit/"reproduce"))
        spec = importlib.util.spec_from_file_location("saved",kit/"rr-test/saved.py")
        source_module=importlib.util.module_from_spec(spec);sys.modules["saved"]=source_module;spec.loader.exec_module(source_module)
        source=source_module.Source(kit)
        spec=importlib.util.spec_from_file_location("rr_original_checks",kit/"rr-test/evaluate-final.py")
        checks=importlib.util.module_from_spec(spec);spec.loader.exec_module(checks)
        for model in expected:
            row=source.model(model);prepared=root/(model+"-prepared");evaluated=root/(model+"-evaluated")
            process=None;prepared_hashes=None
            try:
                subprocess.run(command(kit,model,prepared,prepare=True),check=True)
                owned[prepared]=(prepared.stat().st_dev,prepared.stat().st_ino)
                check_working_copy(prepared,row,source)
                prepared_hashes=input_file_hashes(prepared,row)
                require(not (prepared/"evaluated.par").exists(), "Preparation executed a model")
                report["prepared_models"].append(model);clean(prepared)
                process=subprocess.run(command(kit,model,evaluated),text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
                if evaluated.is_dir() and not evaluated.is_symlink(): owned[evaluated]=(evaluated.stat().st_dev,evaluated.stat().st_ino)
                require(process.returncode==0, "Saved-PAR helper failed: " + process.stdout[-4000:])
                check_working_copy(evaluated,row,source)
                receipt=json.loads(read_regular(evaluated/"evaluation-check.json"))
                native=parse_native_log(read_regular(evaluated/"mfcl-evaluation.log"))
                original=source.read(row["final_par"]);actual=read_regular(evaluated/"evaluated.par")
                saved_objective=checks.scalar(original,"# Objective function value")
                evaluated_objective=checks.scalar(actual,"# Objective function value")
                require(checks.scalar(actual,"# The number of parameters")==1997, "Evaluated parameter count differs")
                require(digest(read_regular(evaluated/"input.par"))==row["final_par"]["sha256"], "Staged PAR changed")
                check_receipt(receipt,row,saved_objective,evaluated_objective,native["first_logged_objective"])
                rep=read_regular(evaluated/"plot-evaluated.par.rep")
                difference=checks.compare_rep(rep,source.read(row["reference_rep"]))
                require(receipt["central_rep_max_abs_diff"]==difference, "Central REP receipt differs")
                for name,data in (("evaluated.par",actual),("plot-evaluated.par.rep",rep)):
                    require(receipt["files"][name]=={"sha256":digest(data),"bytes":len(data)}, "Generated output receipt differs")
                save_json(proof/(model+".evaluation-check.json"),receipt)
                save_json(proof/(model+".native-counter-proof.json"),dict(native,model=model,zip_sha256=ZIP_SHA,
                    central_rep_max_abs_diff=difference,parameters=1997,
                    generated_output_inventory=generated_output_inventory(evaluated,row)))
                report["passed_models"].append(model);verify_zip(zip_path);clean(evaluated)
            except BaseException as error:
                retain_failure(proof,model,evaluated,row,source,checks,process,prepared_hashes,error)
                raise
        require(report["prepared_models"]==list(expected)==report["passed_models"], "Shard coverage is incomplete")
        verify_zip(zip_path);report["status"]="passed";report["zip_unchanged"]=True
        report["kit_inventory_sha256"]=inventory
    except BaseException as error:
        report["failure"]=str(error);raise
    finally:
        save_json(proof/"shard.json",report)
        # Only captured fresh case folders are removed. The original ZIP,
        # repository, extracted kit and failure proof remain available.
        for path in list(owned): clean(path)


def aggregate(directory, proof):
    proof=proof.parent.resolve()/proof.name
    require(ROOT.resolve() not in proof.parents and proof!=ROOT.resolve(), "Proof must be outside repository")
    proof.mkdir(mode=0o700)
    result={"schema_version":1,"status":"failed","zip_sha256":ZIP_SHA,"engine_sha256":ENGINE_SHA,
            "function_evaluation_ceiling":1,"reported_iteration":0,"reported_function_counter":0,"commit":os.environ.get("GITHUB_SHA","UNKNOWN")}
    try:
        require(os.environ.get("NATIVE_MATRIX_RESULT")=="success", "Native matrix jobs did not all pass")
        data=verify_zip(ROOT/"rr-test/standalone.zip")
        with zipfile.ZipFile(io.BytesIO(data)) as archive:
            manifest=json.loads(archive.read("bet-2026-rr-standalone/manifest.json"))
        models={row["model"]:row for row in manifest["models"]}
        require(tuple(sorted(models))==MODELS and len(models)==60, "Frozen model membership differs")
        paths=sorted(directory.glob("**/shard.json"))
        reports=[json.loads(read_regular(path)) for path in paths]
        require(len(reports)==3 and {row["shard"] for row in reports}=={0,1,2}, "Expected three distinct shard proofs")
        coverage=[]
        for path,row in zip(paths,reports):
            expected=list(shard_models(row["shard"]))
            require(row["status"]=="passed" and row["zip_sha256"]==ZIP_SHA and row["engine_sha256"]==ENGINE_SHA
                    and row["zip_unchanged"] is True and row["expected_models"]==expected
                    and row["prepared_models"]==expected and row["passed_models"]==expected
                    and row["commit"]==result["commit"], "Shard proof is incomplete or differs")
            for model in expected:
                receipt=json.loads(read_regular(path.parent/(model+".evaluation-check.json")))
                native=json.loads(read_regular(path.parent/(model+".native-counter-proof.json")))
                require(native["model"]==model and native["zip_sha256"]==ZIP_SHA
                        and type(native["function_evaluation_ceiling"]) is int and native["function_evaluation_ceiling"]==1
                        and native["reported_iteration"]==0 and native["reported_function_counter"]==0 and native["parameters"]==1997
                        and native["all_native_counters_1997_0_0"] is True
                        and native["native_control_records"]>=1 and native["native_counter_records"]>=1
                        and re.fullmatch(r"[0-9a-f]{64}",native["native_log_sha256"]) is not None,
                        "Per-case native proof differs")
                snippets="\n".join(native["native_control_snippets"]+native["native_counter_snippets"]
                                   +[f"Total func {native['first_logged_objective']:.17g}"])
                parse_native_log(snippets.encode())
                check_receipt(receipt,models[model],models[model]["saved_objective"],
                              receipt["evaluated_objective"],native["first_logged_objective"])
                require(native["central_rep_max_abs_diff"]==receipt["central_rep_max_abs_diff"]
                        and math.isfinite(native["central_rep_max_abs_diff"]) and native["central_rep_max_abs_diff"]>=0,
                        "Central REP proof differs")
                check_output_inventory(native["generated_output_inventory"],models[model],receipt)
            coverage.extend(row["passed_models"])
        require(len(coverage)==len(set(coverage))==60 and tuple(sorted(coverage))==MODELS, "Aggregate must cover exactly 60 cases")
        result.update(status="passed",models=sorted(coverage),count=60)
    except BaseException as error:
        result["failure"]=str(error);raise
    finally:save_json(proof/"all-60.json",result)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--shard",type=int);parser.add_argument("--aggregate",type=Path)
    parser.add_argument("--proof-dir",type=Path,required=True)
    parser.add_argument("--work-parent",type=Path)
    args=parser.parse_args()
    require((args.shard is None)!=(args.aggregate is None), "Choose one shard or aggregation")
    if args.aggregate is not None: aggregate(args.aggregate,args.proof_dir)
    else:
        require(sys.platform=="linux" and os.uname().machine=="x86_64", "Native CI requires Linux x86-64")
        require(args.work_parent is not None and args.work_parent.is_dir(), "Existing external work parent required")
        run_shard(args)


if __name__=="__main__":
    try: main()
    except (ValueError,OSError,KeyError,subprocess.CalledProcessError) as error:sys.exit(str(error))
