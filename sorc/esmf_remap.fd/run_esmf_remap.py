#! /usr/bin/env python3

# Test script to run the esmf_remap application.
import os
import sys

sys.path.append(os.path.join(os.getcwd(), "ush"))
from esmf_remap import ESMFRemap

yaml_file = sys.argv[1]

remap = ESMFRemap(yaml_file=yaml_file)
remap.run()
