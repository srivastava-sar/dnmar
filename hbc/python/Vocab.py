class Vocab:
    def __init__(self, vocabFile=None):
        self.count = True
        self.closed = False
        self.unk = 1
        self.nextId = 2
        self.wordCount = {}
        self.word2id = {}
        self.id2word = {}
        if vocabFile:
            for line in open(vocabFile):
                line = line.rstrip('\n')
                (word, wid, count) = line.split('\t')
                self.word2id[word] = int(wid)
                self.id2word[wid] = word
                self.wordCount[word] = float(count)
                self.nextId = max(self.nextId, int(wid) + 1)

    def GetID(self, word):
        if self.closed and not self.word2id.has_key(word):
            return self.unk
        if not self.word2id.has_key(word):
            self.word2id[word] = self.nextId
            self.nextId += 1
        if self.count:
            self.wordCount[word] = self.wordCount.get(word, 0.0) + 1.0
        return self.word2id[word]

    def HasWord(self, word):
        return self.word2id.has_key(word)

    def HasId(self, wid):
        return self.id2word.has_key(wid)

    def GetWord(self, wid):
        return self.id2word[wid]
    
    def GetCount(self, word):
        return self.wordCount[word]

    def SaveVocab(self, vocabFile):
        fOut = open(vocabFile, 'w')
        for word in self.word2id.keys():
            fOut.write("%s\t%s\t%s\n" % (word, self.word2id[word], self.wordCount[word]))

    def GetVocabSize(self):
        return self.nextId-1
