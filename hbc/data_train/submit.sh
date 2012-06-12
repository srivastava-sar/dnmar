rm dictionary_files/*
python ../python/protoDump2Hbc.py --nGC=5 --dictDirOut=dictionary_files --entityFeatureOut=entity_feature --protoDumpIn=../../data/train
ls dictionary_files | sort > dict
cat entity_feature | python ../python/entityWords2hbc.py --dictDir=dictionary_files > docs.hbc
../models/LabeledLDA_64.out docs.hbc labels 1000 50 > samples
python ../python/hbc2topicDistributions.py --sampleFile=samples --docFile=docs.hbc --entityFile=entities --topicWordOut=topic-word --entityTopicOut=entity-topic
cat samples | /usr/bin/python ../python/samples2hbc.py z1.hbc 1
