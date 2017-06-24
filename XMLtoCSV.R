library(XML)
library(plyr)
library("gridExtra")
setwd("/home/harshita/Desktop/Max Planck/Experiment5/TrainingData")
#List of all filenames
inputXMLFileNames <- file("inputxmlfilenames.txt",open="r")
input_xml_file_name_list <-readLines(inputXMLFileNames)
close(inputXMLFileNames)
check = 0

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

