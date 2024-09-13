install.packages("rmarkdown")
install.packages("knitr")

##We can write anything we want in a markdown
#its technically an editor for text ,imagery , video and r code

#The pound sign(#) is for headings .. you write the pound sign , space , then the text
# The headings reduce respectively with increase in pound signs ...

# To edit headings in :-
  #italic ..its # *Italic*
  #bold ..its # **Bold**
  #both .. its # ***Bold italic***

#You can also add links to your markdown

##to tell the markdown your inserting  code...we use `` quotes 
##eg the word plot is now code to markdown `plot`
#for r we type in
#```{r}``` were the `` are opening and closing tags type shi
#in plotting this ~ refers to against. 
#plot(data1$speed ~ data1$dist)