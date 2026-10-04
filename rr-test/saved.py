#!/usr/bin/env python3
"""One checked source for saved native files and exact central REP sections."""
import argparse
import csv
import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[1]
INPUTS = ('bet.frq','bet.ini','bet.tag','bet.age_length','bet.reg_scaling','mfcl.cfg')


class Source:
    def __init__(self, root=ROOT):
        self.root = Path(root).resolve()
        folder = self.root / 'reproduce'
        spec = importlib.util.spec_from_file_location('native_restore', folder/'restore.py')
        self.tool = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.tool)
        with self.tool.frozen_bytes(self.root/'rr-test/native-models.json') as data:
            self.index = json.loads(data)
        with self.tool.frozen_bytes(folder/'package.json') as data:
            if hashlib.sha256(data).hexdigest() != self.index['package_sha256']:
                raise ValueError('Native package recipe differs from model index')
            self.recipe = json.loads(data)
        self.manifest = self.tool.read_json(self.recipe['manifest'])
        self.closure = self.tool.read_json(self.recipe['closure'])
        self.payload = self.tool.archive_files(self.recipe, self.manifest)
        self.members = {row["path"]:row for row in self.manifest["files"]}
        self.models = {row['model']:row for row in self.index['models']}
        if (len(self.models) != len(self.index['models']) or len(self.models) != 110
                or set(self.models) != {row['model'] for row in self.closure['models']}
                or any(not re.fullmatch(r'(ensemble-[0-9]{3}|rrtest-[0-9]{3}-rr1)',name) for name in self.models)):
            raise ValueError('Expected 80 original retained models and 30 completed RR1 models')
        with (self.root/'data/ensemble/retained-final-par-manifest.csv').open(newline='') as stream:
            original = {row['ensemble_id'] for row in csv.DictReader(stream)}
        with (self.root/'rr-test/results/run-manifest.csv').open(newline='') as stream:
            completed = {row['rr1_id'] for row in csv.DictReader(stream) if row['rr1_status']=='completed'}
        if len(original)!=80 or len(completed)!=30 or set(self.models)!=original|completed:
            raise ValueError('Saved native model membership differs from original retained/complete ledgers')

    def model(self, name):
        if name not in self.models:
            raise ValueError('No saved final PAR for this model; failed fits cannot regenerate outputs')
        return self.models[name]

    def read(self, record):
        if record['storage'] == 'archive':
            name = record['member']
            actual = self.members[name]
            if (record["sha256"] != actual["sha256"] or int(record["bytes"]) != actual["bytes"]
                    or ("mode" in record and record["mode"] != actual["mode"])):
                raise ValueError("Archive member binding differs from checked manifest")
            data = self.payload[name]
        elif record['storage'] == 'repository':
            name = record['path']
            with self.tool.frozen_bytes(self.root/self.tool.safe_path(name)) as value:
                data = value
        else:
            raise ValueError('Unknown saved-file storage')
        return self.tool.checked(data,dict(record,path=name,bytes=int(record['bytes'])))

    def input_hashes(self, name):
        row = self.model(name)
        expected = row['native_input_sha256']
        if set(expected) != set(INPUTS):
            raise ValueError('Exactly six native input hashes required')
        actual = {file:hashlib.sha256(self.read(row['files'][file])).hexdigest() for file in INPUTS}
        if actual != expected:
            raise ValueError('Saved native inputs differ from the original hashes')
        binding = row.get('input_manifest')
        if binding:
            with self.tool.frozen_bytes(self.root/self.tool.safe_path(binding['path'])) as data:
                if hashlib.sha256(data).hexdigest() != binding['sha256']:
                    raise ValueError('Original native input manifest checksum differs')
            original = dict((file,sha) for sha,file in (line.split(None,1) for line in data.decode().splitlines()))
            if any(expected[file] != original.get(file) for file in INPUTS):
                raise ValueError('Saved native input binding differs from original input manifest')
        return expected

    def native_files(self, name):
        row = self.model(name)
        self.input_hashes(name)
        files = {file:(self.read(row['files'][file]),row['files'][file]['mode'])
                 for file in (*INPUTS,'doitall.sh')}
        files['final.par'] = (self.read(row['final_par']),row['final_par']['mode'])
        return files

    def new_output(self, output):
        output = Path(output)
        if os.path.lexists(output):
            raise ValueError('Existing output directory refused')
        output = output.resolve()
        if output == self.root or (self.root in output.parents and self.root/'outputs' not in output.parents):
            raise ValueError('Output inside the checkout must be beneath outputs/')
        return output

    def restore(self, name, output, engine=None):
        output = self.new_output(output)
        files = self.native_files(name)
        if engine is not None:
            with self.tool.frozen_bytes(engine) as data:
                record = self.recipe['engine']
                files['mfclo64'] = (self.tool.checked(data,record),0o755)
        self.tool.save_files(output,files,name)

    def summary_files(self, output):
        output = self.new_output(output)
        files, table = {}, []
        for name,row in sorted(self.models.items()):
            self.input_hashes(name)
            par,rep = self.read(row['final_par']),self.read(row['reference_rep'])
            if name.startswith('rrtest-'):
                par_name,rep_name = name+'--final.par',name+'--central.rep'
                files[par_name]=(par,0o600);files[rep_name]=(rep,0o600)
                par_path,rep_path = output/par_name,output/rep_name
            else:
                par_path,rep_path = self.root/row['final_par']['path'],self.root/row['reference_rep']['path']
            table.append({'model':name,'final_par':str(par_path),'rep':str(rep_path)})
        text = io.StringIO();writer=csv.DictWriter(text,fieldnames=['model','final_par','rep'])
        writer.writeheader();writer.writerows(table)
        files['models.csv']=(text.getvalue().encode(),0o600)
        self.tool.save_files(output,files,'paired-summary')


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    commands=parser.add_subparsers(dest='action',required=True)
    commands.add_parser('verify')
    summary=commands.add_parser('summary-files');summary.add_argument('output',type=Path)
    restore=commands.add_parser('restore');restore.add_argument('model');restore.add_argument('output',type=Path)
    args=parser.parse_args();source=Source()
    if args.action=='verify':
        for name in source.models:
            source.native_files(name);source.read(source.model(name)['reference_rep'])
        print('Verified exact native files and central references for 80 retained models and 30 RR1 fits.')
    elif args.action=='summary-files':source.summary_files(args.output)
    else:source.restore(args.model,args.output)


if __name__=='__main__':
    try:main()
    except (ValueError,OSError,KeyError) as error:sys.exit(str(error))
