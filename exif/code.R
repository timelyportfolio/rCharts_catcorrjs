#exiftools is really powerful
#http://www.cameratechnica.com/2012/05/23/hack-your-exif-data-from-the-command-line-five-fun-uses-for-exiftool/


#there are 2 R packages that purport to look at exif
#adimpro but exif very limited and I could not get to work
#plotKML but only for wikimedia commons images

#hackiest way to do is just use exiftool through system
#write to temporary text file and then read
#example to see full info on a single photo
system('exiftool -h ./IMG_3555.JPG')


#examine iso across all
iso.df <- data.frame(
  system('exiftool -T -r -ISO .',inter=TRUE),
  stringsAsFactors = FALSE
)
#do a combination
info <- system('exiftool -T -r -ISO -ShutterSpeed -CameraTemperature -DateTimeOriginal .',inter=TRUE)
img.df <- read.delim2(
  textConnection(info),
  stringsAsFactors = FALSE,
  header = FALSE,
  col.names = c("ISO", "ShutterSpeed", "CameraTemp", "Date")
)
#get just date
img.df[,4] <- as.Date(#as.POSIXct(
  paste0(
    gsub(x=substr(img.df[,4],1,10),pattern=":",replacement="-"),
    substr(img.df[,4],11,19)
  )
)
img.df$id = 1:NROW(img.df)
require(vcdExtra)
mosaic(structable(img.df[,c(1,2)]))
assoc(img.df[,c(1,2)],shade=T)
plot(x=img.df$ISO, y=lapply(img.df[,2],FUN=function(x){return(eval(parse(text=x)))}))

require(rCharts)
d1 <- dPlot(
  x = "ISO",
  y = "ShutterSpeed",
  z = "Freq",
  groups = c("ISO"),
  data = subset(
    data.frame(structable(ISO~ShutterSpeed, data = img.df)),
    Freq > 0
  ),
  type = "bubble"
)
d1$yAxis(type = "addCategoryAxis")
d1$zAxis(type = "addMeasureAxis", overrideMax = NROW(img.df)/1.5)
d1

d2 <- dPlot(
  x = "ISO",
  y = "Freq",
  #z = "Freq",
  groups = c("ShutterSpeed"),
  data = subset(
    data.frame(structable(ISO~ShutterSpeed, data = img.df)),
    Freq > 0
  ),
  type = "bar"
)
d2$xAxis(orderRule = "ISO")
#d2$yAxis(type = "addPctAxis")
d2


#remove iso speeds that are not numeric
#manual for now
img.df <- img.df[-(which(is.na(as.numeric(img.df[,1])))),]
img.df$Date <- format(img.df$Date)


catCorrPlot <- function(questions, responses){
  require(rCharts)
  #responses = read.csv(responses_doc)
  responses = toJSONArray(setNames(
    responses[,-1], 1:(NCOL(responses) - 1)
  ), json = F)
  #questions = read.csv(questions_doc, stringsAsFactors = F)
  questions = lapply(1:NROW(questions), function(i){
    qi = as.list(questions[i,])
    qi$choices = strsplit(qi$choices, ";")[[1]]
    qi$number = i
    qi
  })
  questions = toJSONArray(questions, json = F)
  r1 <- rCharts$new()
  r1$setLib('http://timelyportfolio.github.io/howitworks/catcorrjs/catcorrjs')
  r1$set(questions = questions, responses = responses)
  r1
}

responses <- img.df[,c(4,4,1,2)]
questions <- do.call(rbind,lapply(1:2,function(x){
  choices <- unique(img.df[,x])
  choices <- choices[order(unlist(lapply(choices,function(x){
    as.numeric(eval(parse(text=x)))
  })))]
  return(data.frame(
      "outcome",
      colnames(img.df)[x],
      capture.output(cat(choices,sep=";")),
      stringsAsFactors = F
    )
  )
  }
))
colnames(questions) <- c("type","text","choices")
questions <- rbind(questions,c("demographic","Date", capture.output(cat(unique(img.df[,4]),sep=";"))))
questions <- questions[c(3,1,2),]   

r1 <- catCorrPlot(questions, responses)
r1