#!/usr/bin/env python3
import re
import os
import sys
import glob

# Constants
struct_def_pattern = r"typedef struct\s?{[\s\S]*?}\s?(\w+)\s?(?:#\(.*?\))?\s*?(?:deriving[\s\S]*?)?;"
enum_def_pattern = r"typedef enum\s?{[\s\S]*?}\s?(\w+)\s?(?:#\(.*?\))?\s*?(?:deriving[\s\S]*?)?;"

if __name__ == '__main__':
    print("BSV Ctags generator 0.1.0")
    print("=========================")

    try:
        print("Root dir passed {}".format(sys.argv[1]))
    except IndexError as e:
        print("Root dir not passed")
        exit(1)

    bsc_files = glob.glob(os.path.abspath(sys.argv[1]) + '/**/*.bsv', recursive=True)

    print("Num files found = {}".format(len(bsc_files)))

    tags = []

    # Iterate over bsv files
    for item in bsc_files:
        #print("Parsing file: {}".format(item))
        with open(item, 'r') as f:
            code = f.read()

            # Struct matches
            matches = re.finditer(struct_def_pattern, code)
            for match in matches:
                tags.append((os.path.relpath(item), match.groups()[0]))

            # Enum Matches
            matches = re.finditer(enum_def_pattern, code)
            for match in matches:
                tags.append((os.path.relpath(item), match.groups()[0]))

    # Sort the tags
    tags_sorted = sorted(tags, key = lambda x: x[1])

    with open('tags', 'w') as f:
        for item in tags_sorted:
            f.write('{0}\t{1}\t{2}\n'.format(
                item[1], item[0],
                r"/}}\s\{{-}}{}/".format(item[1])))
