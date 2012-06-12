#/usr/bin/python

#######################################################################################################3
# Script for evaluating aggregate Precision/Recall of distant supervision against a set of dictionaries
# / KB.  As discussed by Mintz/Hoffmann/etc... this underestimates percision because Freebase is
# incomplete.  Also there are errors due to entity linking, etc...
#######################################################################################################3

import sys
import os
import re

id2dict = {}
dict2id = {}

ALPHA=1.0
THRESHOLD=0.2

def pDist(x, dist):
    s = sum(dist.values())
    return float(dist[x]) / float(s)

i = 1
for line in open('dictionaries'):
    line = line.strip()
    id2dict[i] = line
    dict2id[line] = i
    i += 1

dictionaries = set()
for d in dict2id.keys():
    if re.search(r'^GC', d):
        continue
    for line in open('dictionary_files/%s' % d):
        entry = line.strip()
        #print "%s\t%s" % (entry, d)
        dictionaries.add("%s\t%s" % (entry, d))

tp = 0.0
fp = 0.0

for line in open('entity-topic'):
    fields = line.strip().split('\t')
    entry = fields[0]
    dCounts = {}

    predicted = [id2dict[int(x.split(':')[0])] for x in fields[1:]]
    if len(predicted) == 0 or re.search(r'^GC', predicted[0]):
        continue

    for d in dict2id.keys():
        dCounts[d] = ALPHA

    for f in fields[1:]:
        (did, count) = f.split(':')
        did = int(did)
        dCounts[id2dict[did]] = dCounts.get(id2dict[did], 0.0) + float(count)

    #print pDist(predicted[0], dCounts)
    for p in [predicted[0]]:
        if pDist(p, dCounts) >= THRESHOLD:
            #print "%s\t%s" % (entry, predicted[0])
            if "%s\t%s" % (entry, p) in dictionaries:
                tp += 1.0
                #print "%s\t%s" % (entry, predicted[0])
            else:
                fp += 1.0
                #print "%s\t%s" % (entry, p)

fn = len(list(dictionaries)) - tp

print "tp = %s" % tp
print "fp = %s" % fp
print "fn = %s" % fn

P = tp / (tp + fp)
R = tp / (tp + fn)

print "P = %s" % P
print "R = %s" % R
print "F = %s" % (2 * P * R / (P + R))

