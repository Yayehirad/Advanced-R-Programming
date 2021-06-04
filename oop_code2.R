#Part 2: Longitudinal Data Class and Methods
#set Generics
setGeneric("print")
setGeneric("summary")
setGeneric("subject", function(x,...){
  standardGeneric("subject")
})
setGeneric("visit", function(x,...){
  standardGeneric("visit")
})
setGeneric("room", function(x,...){
  standardGeneric("room")
})
# Create a make_LD class for the longitudinal data
setClass("make_LD",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))
# Create methods for the longitudinal  make_LD class
setMethod("print",
          c(x = "make_LD"),
          function(x){
            paste("Longitudinal dataset with", length(unique(x@id)), "subjects")
          })
setMethod("subject",
          c(x = "make_LD"),
          function(x,n){
            new("subject_class", id = x@id[x@id == n], visit = x@visit[x@id == n],
                room = x@room[x@id == n], value = x@value[x@id == n],
                timepoint = x@timepoint[x@id == n])
          })

# Create a function that converts a dataframe to the make_LD class by putting the columns in the dataframe into the slots for the S4 class
make_LD <- function(x) {
  new("make_LD", id = x$id, visit = x$visit,
      room = x$room, value = x$value, timepoint = x$timepoint)
}
# Creating the subject_class class 
setClass("subject_class",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))
# Creating the subject_class methods
setMethod("print",
          c(x = "subject_class"),
          function(x){
            if (length(unique(x@id)) > 0) {
              cat(paste("Subject ID:",unique(x@id)))
            } else {
              NULL
            }
          })
setMethod("summary",
          c(object = "subject_class"),
          function(object){
            new("subject_summary", id = object@id, visit = object@visit, 
                room = object@room, value = object@value)
          })
setMethod("visit",
          c(x = "subject_class"),
          function(x,n){
            new("visit_class", id = x@id[x@visit == n], visit = x@visit[x@visit == n],
                room = x@room[x@visit == n], value = x@value[x@visit == n],
                timepoint = x@timepoint[x@visit == n])
          })
# Creating subject_summary class and methods so that we can get summary statistics
setClass("subject_summary",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric"))
setMethod("print",
          c(x = "subject_summary"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            as.data.frame(cbind(visit=x@visit,room=x@room,value=x@value),stringsAsFactors = FALSE) %>%
              mutate(value = as.numeric(value)) %>%
              group_by(visit,room) %>%
              summarise(avg = mean(value)) 
          })
# now for vists 
setClass("visit_class",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))
setMethod("room",
          c(x = "visit_class"),
          function(x,n){
            new("room_class", id = x@id[x@room == n], visit = x@visit[x@room == n],
                room = x@room[x@room == n], value = x@value[x@room == n],
                timepoint = x@timepoint[x@room == n])
          })
# Now the room_class class and methods
setClass("room_class",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))
setMethod("print",
          c(x = "room_class"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            cat(paste("Visit:",unique(x@visit)),"\n")
            cat(paste("Room:",unique(x@room)))
          })
setMethod("summary",
          c(object = "room_class"),
          function(object){
            new("room_summary", id = object@id, value = object@value)
          })
# room summary stats with the room_summary class and methods
setClass("room_summary",
         representation(id = "numeric", 
                        value = "numeric"))
setMethod("print",
          c(x = "room_summary"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            summary(x@value)
          })
