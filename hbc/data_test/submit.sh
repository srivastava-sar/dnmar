mkdir -p dictionary_files
rm dictionary_files/*

#Need to make sure we have the same set of dictionaries....
cp ../data_train/dictionary_files/* dictionary_files/

echo 'protoDump2Hbc'
python ../python/protoDump2Hbc.py --nGC=5 --dictDirOut=dictionary_files --entityFeatureOut=entity_feature --protoDumpIn=../../data/test
#NOTE: should really just have one set of dictionaries for use in both training and test, but this is just easier...  Also fine because we can't really do anything about realations not seen in training...
rm dictionary_files/@base@locations@countries@states_provinces_within
rm dictionary_files/@location@country@languages_spoken
ls dictionary_files | sort > dict

echo 'entityWords2hbc'
cat entity_feature | python ../python/entityWords2hbc.py --dictDir=dictionary_files --vocab=../data_train/vocab --test > test.hbc

#I think none of the entities from the training data will be observed in test data for this scenario... 
echo 'entityMap'
for x in `cat entities`
do
    echo '-1' >> entityMap
done

echo 'Predict'
python ../python/NBC.py --topicWord=../data_train/topic-word --entityTopic=../data_train/entity-topic > predictions.out

echo 'Compute P/R'
python ../python/aggregateScore_predicted.py > PRF

#echo 'LabeledLDA_infer'
#../models/LabeledLDA_infer.out ../data_train/docs.hbc ../data_train/z1.hbc test.hbc labels entityMap 100 100 > samples
