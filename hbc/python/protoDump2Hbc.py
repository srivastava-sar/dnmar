#!/usr/bin/python

###################################################################################
# protoDump2Hbc.py
#
# Converts text format dump from probtobuf format into hbc format
# (don't have the .proto file, only the java class to read it...)
# Text dump seems like the easiest way to deal with this...
###################################################################################

import sys
import re
import random

from optparse import OptionParser

parser = OptionParser()
parser.add_option("--protoDumpIn", dest="protoDumpIn")
parser.add_option("--dictDirOut", dest="dictDirOut")
parser.add_option("--sample10p", dest="sample10p", action="store_false", default=True)
parser.add_option("--entityFeatureOut", dest="entityFeatureOut")
parser.add_option("--nGC", type="int", dest="nGC")
(options, args) = parser.parse_args()

nextLabel = 0

sourceGuid = None
destGuid   = None
relType    = None
features   = []

efOut = open(options.entityFeatureOut, 'w')
for line in open(options.protoDumpIn):
    line = line.strip()

    m = re.search(r'sourceGuid: "([^"]+)"', line)
    if m:
        if relType:
            #Print out the previous example
            for f in features:
                efOut.write("%s-%s\t%s\n" % (sourceGuid, destGuid, f))
            
            relations = [x for x in relType.split(',') if x != 'NA']
            gc        =  ['GC%s' % x for x in range(1,options.nGC+1)]

            #Sample 10% of negatives as done by Reidel et. al.
            if options.sample10p or len(relations) > 0 or random.uniform(0,1) < 0.1:
                for r in relations + gc:
                    dOut = open(options.dictDirOut + "/" + r.replace('/', '@'), 'a')
                    dOut.write("%s-%s\n" % (sourceGuid, destGuid))
                    dOut.close()

            #Clear out features for new entity pair (and everything else to be careful) 
            sourceGuid = None
            destGuid   = None
            relType    = None
            features   = []

        sourceGuid = m.group(1)
        continue
    m = re.search(r'destGuid: "([^"]+)"', line)
    if m:
        destGuid = m.group(1)
        continue
    m = re.search(r'relType: "([^"]+)"', line)
    if m:
        relType = m.group(1)
        continue
    m = re.search(r'feature: "([^"]+)"', line)
    if m:
        features.append(m.group(1))
        continue
