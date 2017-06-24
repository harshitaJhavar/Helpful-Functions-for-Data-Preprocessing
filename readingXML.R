library(XML)
library(plyr)
library("ggplot2")
library("gridExtra")
setwd("/home/harshita/Desktop/Max Planck/Experiment5/TrainingData")
#List of all filenames
inputXMLFileNames <- file("inputxmlfilenames.txt",open="r")
input_xml_file_name_list <-readLines(inputXMLFileNames)
close(inputXMLFileNames)
check = 0
#Reading Train File
trainData <- read.csv("trainFile.csv")
#Converting factor to string character for all entries in train Data
trainData$Emotion <- as.character(trainData$Emotion)
trainData$Character1 <- as.character(trainData$Character1)
trainData$Character2 <- as.character(trainData$Character2)
trainData$Summary <- as.character(trainData$Summary)

#The below function takes a list of xml file names and returns the templates with coreferences replaced by the character name
create_template_from_replacing_coreference <- function(sentence_id_list,sentence_text_list){
 
  #Replacing char1 value in summary with char1 and char2 value in summary with char2 to 
  #create a template out of summary text.

  #For Train Data
  for (i in 1:418){
    #Using regular expression to find the first name or last name in the text.
    separateFirstMiddleLastNameOfCharacter1 <- strsplit(trainData$Character1[i],"\\s+|-|\\.")
    separateFirstMiddleLastNameOfCharacter2 <- strsplit(trainData$Character2[i],"\\s+|-|\\.")
    #Replacing the coreferences
    #If any part of the split of the character1 name occurs in any of the text, then, we replace that piece of text with character1
    if(separateFirstMiddleLastNameOfCharacter1[[1]][[1]] %in% sentence_text_list){
      #Replace each corresponding text in that particular sentence whose id is present in sentence_id
      for(q in 1:length(sentence_id_list)){
        trainData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character1", trainData$Summary[[1]][[q]])
      }}
      #If any part of the split of the character1 name occurs in any of the text, then, we replace that piece of text with character1
      if(length(separateFirstMiddleLastNameOfCharacter1)==2 && separateFirstMiddleLastNameOfCharacter1[[1]][[2]] %in% sentence_text_list){
        #Replace each corresponding text in that particular sentence whose id is present in sentence_id
        for(q in 1:length(sentence_id_list)){
          trainData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character1", trainData$Summary[[1]][[q]])
        }}
        if(length(separateFirstMiddleLastNameOfCharacter1)==3 && separateFirstMiddleLastNameOfCharacter1[[1]][[3]] %in% sentence_text_list){
          #Replace each corresponding text in that particular sentence whose id is present in sentence_id
          for(q in 1:length(sentence_id_list)){
            trainData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character1", trainData$Summary[[1]][[q]])
          }  
        } 
    #For character 2
      #If any part of the split of the character2 name occurs in any of the text, then, we replace that piece of text with character2
      if(separateFirstMiddleLastNameOfCharacter2[[1]][[1]] %in% sentence_text_list){
        #Replace each corresponding text in that particular sentence whose id is present in sentence_id
        for(q in 1:length(sentence_id_list)){
          trainData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character2", trainData$Summary[[1]][[q]])
        }}
        #If any part of the split of the character2 name occurs in any of the text, then, we replace that piece of text with character2
        if(length(separateFirstMiddleLastNameOfCharacter2)==2 && separateFirstMiddleLastNameOfCharacter2[[1]][[2]] %in% sentence_text_list){
          #Replace each corresponding text in that particular sentence whose id is present in sentence_id
          for(q in 1:length(sentence_id_list)){
            trainData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character2", trainData$Summary[[1]][[q]])
          }}
        if(length(separateFirstMiddleLastNameOfCharacter2)==3 && separateFirstMiddleLastNameOfCharacter2[[1]][[3]] %in% sentence_text_list){
          #Replace each corresponding text in that particular sentence whose id is present in sentence_id
          for(q in 1:length(sentence_id_list)){
            trainData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character2", trainData$Summary[[1]][[q]])
          }  
        } 
      
    #Replacing any of first name or last name with char1 or char2 respectively to create a template
    
    #Replacing the first name of Character 1
    trainData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter1[[1]][[1]], "Character1", trainData$Summary[i])
    #Replacing the middle name of Character 1
    if(length(separateFirstMiddleLastNameOfCharacter1)==2){
      trainData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter1[[1]][[2]], "Character1", trainData$Summary[i])
    }
    if(length(separateFirstMiddleLastNameOfCharacter1)==3){
      trainData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[2]], "Character1", trainData$Summary[i])
      trainData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[3]], "Character1", trainData$Summary[i])
    } 
    #Replacing the first name of Character 2
    
    trainData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[1]], "Character2", trainData$Summary[i])
    #Replacing the middle name of Character 2
    if(length(separateFirstMiddleLastNameOfCharacter2)==2){
    trainData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[2]], "Character2", trainData$Summary[i])
    }
    #Replacing the middle and last name of Character 2
    if(length(separateFirstMiddleLastNameOfCharacter2)==3){
      trainData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[2]], "Character2", trainData$Summary[i])
      trainData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[3]], "Character2", trainData$Summary[i])
    }
    trainData$Summary[i]=gsub("\n", " ", trainData$Summary[i])
  }
#  print(trainData$Summary[[1]][[1]][[1]])
  
       #Retrieving only those sentences in which both Character1 and Character2 are coming.
  #For Train Data
  for (i in 1:418){
    tempTxt<- strsplit(trainData$Summary[i],"\\.|\\?")
    #Looking for only those sentences which have character 1 and character 2 in them.
    b <-grep("Character1.*Character2|Character2.*Character1", tempTxt[[1]], ignore.case=TRUE, value= TRUE)
    #Assigning those relevant sentences in train Data Summary section
    trainData$Summary[i]=paste(b, collapse='. ' )
  }
  return(trainData)
}

parseXML <- function(input_xml_file){
  for(k in 1:length(input_xml_file)){
    xmlfile=xmlParse(input_xml_file[k])
    class(xmlfile) #"XMLInternalDocument" "XMLAbstractDocument"
    xmltop = xmlRoot(xmlfile)
    class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
    xmlName(xmltop) #give name of node, PubmedArticleSet
    xmlSize(xmltop) #how many children in node, 31 coreferences in first file
    xmlName(xmltop[[1]]) #name of root's children
    # have a look at the content of the first child entry
    xmltop[[1]]
    #Root Node's children
    #xmlSize(xmltop[[1]]) #number of nodes in each child
    #xmlSApply(xmltop[[1]], xmlName) #name(s)
    #xmlSApply(xmltop[[1]], xmlAttrs) #attribute(s)
    xmlSApply(xmltop[[1]], xmlSize) #size
  
    #The below loop prints the corefernces and corresponding sentence id for each summary
    number_of_coreferences = xmlSApply(xmltop[[1]], xmlSize)[[2]]
    for(j in 1:number_of_coreferences){
      sentence_id_list = NULL
      sentence_text_list = NULL
      
      #Loop for collecting coreference for each word and corresponding sentence id
    for(i in 1:xmlSApply(xmltop[[1]][[2]], xmlSize)[[j]]){
    #[Sentence ID, text]
    sentence_id = xmltop[[1]][[2]][[j]][[i]][[1]][[1]]
    sentence_text = xmltop[[1]][[2]][[j]][[i]][[5]][[1]]
    sentence_id_list = append(sentence_id_list, sentence_id)
    sentence_text_list = append(sentence_text_list, sentence_text)
    }
    #The sentence_id and the sentence_text lists are passed to function create_template_from_replacing_coreference
    trainData <- create_template_from_replacing_coreference(sentence_id_list,sentence_text_list)
    #print(sentence_id)
    #print(sentence_text)
    }
    check = check +1
    print(check)
    }
  return(trainData)
}  

trainData <- parseXML(input_xml_file_name_list)

#Removing the entries which do not have any summaries in their entry
trainData<-trainData[!(trainData$Summary==""),]

#Creating the model
library(stylo)
library(nnet)
#Computing ngrams of the summary 

#For Train Data
septagramsTrainList  = list()
hexagramsTrainList  = list()
pentagramsTrainList  = list()
quadragramsTrainList  = list()
trigramsTrainList  = list()
bigramsTrainList  = list()
emotionList = list()
j=0
for (i in 1:254){
  if(nchar(trainData$Summary[i])>0){
    j=j+1
    emotionList[j] <- trainData$Emotion[i]
    #septagram
    train_septagram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 7))
    septagramsTrainList[j] <- train_septagram_df
    
    #hexagram
    train_hexagram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 6))
    hexagramsTrainList[j] <- train_hexagram_df
    
    #pentagram
    train_pentagram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 5))
    pentagramsTrainList[j] <- train_pentagram_df
    
    #quadragram
    train_quadragram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 4))
    quadragramsTrainList[j] <- train_quadragram_df
    
    #trigram
    train_trigram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 3))
    trigramsTrainList[j] <- train_trigram_df
    
    #bigram
    train_bigram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 2))
    bigramsTrainList[j] <- train_bigram_df
  }
}
#Binding all the n-grams together
bigramsTrainFeature = do.call(rbind, bigramsTrainList)
trigramsTrainFeature = do.call(rbind, trigramsTrainList)
quadragramsTrainFeature = do.call(rbind, quadragramsTrainList)
pentagramsTrainFeature = do.call(rbind, pentagramsTrainList)
hexagramsTrainFeature = do.call(rbind, hexagramsTrainList)
septagramsTrainFeature = do.call(rbind, septagramsTrainList)

#Binding all outcome variable - emotions together
emotionOutcomeList = do.call(rbind, emotionList) 

#Creating the final train table
trainTable <- table(emotionOutcomeList)
trainTable$septagram <- septagramsTrainFeature
trainTable$quadragram <- quadragramsTrainFeature
trainTable$hexagram <- hexagramsTrainFeature
trainTable$pentagram <- pentagramsTrainFeature
trainTable$bigram <- bigramsTrainFeature
trainTable$trigram <- trigramsTrainFeature

#Reading the emotion lexicon list

#Incorporating the emotion lexicon list into the train table
library(MASS)
#Model Building
trainedModelOnNGramsOfSummary <- multinom(emotionOutcomeList ~ trainTable$septagram + trainTable$pentagram + trainTable$trigram, MaxNWts = 6000)

#Looking for model details
#summary(trainedModelOnNGramsOfSummary)

setwd("/home/harshita/Desktop/Max Planck/Experiment5/Test Data")
#Reading Test File
testData <- read.csv("testfile.csv")

#Converting factor to string character for all entries in test Data
testData$Emotion <- as.character(testData$Emotion)
testData$Character1 <- as.character(testData$Character1)
testData$Character2 <- as.character(testData$Character2)
testData$Summary <- as.character(testData$Summary)

#For Test Data
create_template_from_replacing_coreference_TestData <- function(sentence_id_list,sentence_text_list){

#For Test Data, creating a template.
for (i in 1:115){
  #Using regular expression to find the first name or last name in the text.
  separateFirstMiddleLastNameOfCharacter1 <- strsplit(testData$Character1[i],"\\s+|-|\\.")
  separateFirstMiddleLastNameOfCharacter2 <- strsplit(testData$Character2[i],"\\s+|-|\\.")
  #Replacing the coreferences
  #If any part of the split of the character1 name occurs in any of the text, then, we replace that piece of text with character1
  if(separateFirstMiddleLastNameOfCharacter1[[1]][[1]] %in% sentence_text_list){
    #Replace each corresponding text in that particular sentence whose id is present in sentence_id
    for(q in 1:length(sentence_id_list)){
      testData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character1", testData$Summary[[1]][[q]])
    }}
    #If any part of the split of the character1 name occurs in any of the text, then, we replace that piece of text with character1
    if(length(separateFirstMiddleLastNameOfCharacter1)==2 && separateFirstMiddleLastNameOfCharacter1[[1]][[2]] %in% sentence_text_list){
      #Replace each corresponding text in that particular sentence whose id is present in sentence_id
      for(q in 1:length(sentence_id_list)){
        testData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character1", testData$Summary[[1]][[q]])
      }}
    if(length(separateFirstMiddleLastNameOfCharacter1)==3 && separateFirstMiddleLastNameOfCharacter1[[1]][[3]] %in% sentence_text_list){
      #Replace each corresponding text in that particular sentence whose id is present in sentence_id
      for(q in 1:length(sentence_id_list)){
        testData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character1", testData$Summary[[1]][[q]])
      }  
    } 
    #For character 2
    #If any part of the split of the character2 name occurs in any of the text, then, we replace that piece of text with character2
    if(separateFirstMiddleLastNameOfCharacter2[[1]][[1]] %in% sentence_text_list){
      #Replace each corresponding text in that particular sentence whose id is present in sentence_id
      for(q in 1:length(sentence_id_list)){
        testData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character2", testData$Summary[[1]][[q]])
      }}
      #If any part of the split of the character2 name occurs in any of the text, then, we replace that piece of text with character2
      if(length(separateFirstMiddleLastNameOfCharacter2)==2 && separateFirstMiddleLastNameOfCharacter2[[1]][[2]] %in% sentence_text_list){
        #Replace each corresponding text in that particular sentence whose id is present in sentence_id
        for(q in 1:length(sentence_id_list)){
          testData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character2", testData$Summary[[1]][[q]])
        }}
      if(length(separateFirstMiddleLastNameOfCharacter2)==3 && separateFirstMiddleLastNameOfCharacter2[[1]][[3]] %in% sentence_text_list){
        #Replace each corresponding text in that particular sentence whose id is present in sentence_id
        for(q in 1:length(sentence_id_list)){
          testData$Summary[[1]][[q]] = gsub(sentence_text_list[[q]], "Character2", testData$Summary[[1]][[q]])
        }  
      } 
    #Replacing any of first name or last name with char1 or char2 respectively to create a template
    
    #Replacing the first name of Character 1
    testData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter1[[1]][[1]], "Character1", testData$Summary[i])
    #Replacing the middle name of Character 1
    if(length(separateFirstMiddleLastNameOfCharacter1)==2){
      testData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter1[[1]][[2]], "Character1", testData$Summary[i])
    }
    if(length(separateFirstMiddleLastNameOfCharacter1)==3){
      testData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[2]], "Character1", testData$Summary[i])
      testData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[3]], "Character1", testData$Summary[i])
    } 
    #Replacing the first name of Character 2
    
    testData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[1]], "Character2", testData$Summary[i])
    #Replacing the middle name of Character 2
    if(length(separateFirstMiddleLastNameOfCharacter2)==2){
      testData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[2]], "Character2", testData$Summary[i])
    }
    #Replacing the middle and last name of Character 2
    if(length(separateFirstMiddleLastNameOfCharacter2)==3){
      testData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[2]], "Character2", testData$Summary[i])
      testData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2[[1]][[3]], "Character2", testData$Summary[i])
    }
    testData$Summary[i]=gsub("\n", " ", testData$Summary[i])
}
  #  print(trainData$Summary[[1]][[1]][[1]])

#Retrieving only those sentences in which both Character1 and Character2 are coming.
#For Test Data
for (i in 1:115){
  tempTxt<- strsplit(testData$Summary[i],"\\.|\\?")
  #Looking for only those sentences which have character 1 and character 2 in them.
  b <-grep("Character1.*Character2|Character2.*Character1", tempTxt[[1]], ignore.case=TRUE, value= TRUE)
  #Assigning those relevant sentences in test Data Summary section
  testData$Summary[i]=paste(b, collapse='. ' )
}
return(testData)
}

parsetestDataXML <- function(input_xml_file){
  for(k in 1:length(input_xml_file)){
    xmlfile=xmlParse(input_xml_file[k])
    class(xmlfile) #"XMLInternalDocument" "XMLAbstractDocument"
    xmltop = xmlRoot(xmlfile)
    class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
    xmlName(xmltop) #give name of node, PubmedArticleSet
    xmlSize(xmltop) #how many children in node, 31 coreferences in first file
    xmlName(xmltop[[1]]) #name of root's children
    # have a look at the content of the first child entry
    xmltop[[1]]
    #Root Node's children
    #xmlSize(xmltop[[1]]) #number of nodes in each child
    #xmlSApply(xmltop[[1]], xmlName) #name(s)
    #xmlSApply(xmltop[[1]], xmlAttrs) #attribute(s)
    xmlSApply(xmltop[[1]], xmlSize) #size
    
    #The below loop prints the corefernces and corresponding sentence id for each summary
    number_of_coreferences = xmlSApply(xmltop[[1]], xmlSize)[[2]]
    for(j in 1:number_of_coreferences){
      sentence_id_list = NULL
      sentence_text_list = NULL
      
      #Loop for collecting coreference for each word and corresponding sentence id
      for(i in 1:xmlSApply(xmltop[[1]][[2]], xmlSize)[[j]]){
        #[Sentence ID, text]
        sentence_id = xmltop[[1]][[2]][[j]][[i]][[1]][[1]]
        sentence_text = xmltop[[1]][[2]][[j]][[i]][[5]][[1]]
        sentence_id_list = append(sentence_id_list, sentence_id)
        sentence_text_list = append(sentence_text_list, sentence_text)
      }
      #The sentence_id and the sentence_text lists are passed to function create_template_from_replacing_coreference
      testData <- create_template_from_replacing_coreference_TestData(sentence_id_list,sentence_text_list)
      #print(sentence_id)
      #print(sentence_text)
    }
  }
  return(testData)
}  

testData <- parsetestDataXML(input_xml_file_name_list)
#Removing the entries which do not have any summaries in their entry
testData<-testData[!(testData$Summary==""),]
#Computing ngrams of the summary 

#For Test Data
septagramsTestList  = list()
hexagramsTestList  = list()
pentagramsTestList  = list()
quadragramsTestList  = list()
bigramsTestList  = list()
trigramsTestList  = list()
emotionTestList = list()
j=0
for (i in 1:length(testData)){
  if(nchar(testData$Summary[i])>0){
    j=j+1
    emotionTestList[j] <- testData$Emotion[i]
    #nanogram
    test_nanogram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 9))
    nanogramsTestList[j] <- test_nanogram_df
    
    #octagram
    test_octagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 8))
    octagramsTestList[j] <- test_octagram_df
    
    #nanogram
    test_hexagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 6))
    hexagramsTestList[j] <- test_hexagram_df
    
    #septagram
    test_septagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 7))
    septagramsTestList[j] <- test_septagram_df
    
    #pentagram
    test_pentagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 5))
    pentagramsTestList[j] <- test_pentagram_df
    
    #quadragram
    test_quadragram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 4))
    quadragramsTestList[j] <- test_quadragram_df
    
    #trigram
    test_trigram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 3))
    trigramsTestList[j] <- test_trigram_df
    
    #bigram
    test_bigram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 2))
    bigramsTestList[j] <- test_bigram_df
    
  }
}
#Binding all the n-grams together
trigramsTestFeature = do.call(rbind, trigramsTestList)
bigramsTestFeature = do.call(rbind, bigramsTestList)
quadragramsTestFeature = do.call(rbind, quadragramsTestList)
pentagramsTestFeature = do.call(rbind, pentagramsTestList)
septagramsTestFeature = do.call(rbind, septagramsTestList)
hexagramsTestFeature = do.call(rbind, hexagramsTestList)

#Binding all outcome variable - emotions together
emotionOutcomeTestList = do.call(rbind, emotionTestList) 

#Creating the final test table
testTable <- table(emotionOutcomeTestList)
testTable$septagram <- septagramsTestFeature
testTable$pentagram <- pentagramsTestFeature
testTable$trigram <- trigramsTestFeature
testTable$bigram <- bigramsTestFeature
testTable$quadragram <- quadragramsTestFeature
testTable$quadragram <- hexagramsTestFeature

#Reading the emotion lexicon list

#Incorporating the emotion lexicon list into the test table

#Predicting the accuracy of the system
predictedEmotion <- predict(trainedModelOnNGramsOfSummary, newdata = testTable)
# load Caret package for computing Confusion matrix to calculate accuracy
library(caret) 
u = union(predictedEmotion, emotionOutcomeTestList)
t = table(factor(predictedEmotion, u), factor(emotionOutcomeTestList, u))
confusionMatrix(t)
