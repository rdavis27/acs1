library(data.table)
library(ggplot2)
library(plyr)
library(readxl)
library(reshape)
library(RColorBrewer)

library(tidyverse)
library(urbnmapr)
library(ggrepel)
library(maps)

in_shinyapps <- FALSE
occ_codes   <- read.csv("occ_codes.csv")
occ_codes90 <- read.csv("occ1990_codes.csv")
cpiu <- read.csv("cpiu.csv")
occ_select   <- "Programmers, Software developers, app & system SW"
occ_select90 <- "Computer software developers (229)"

shinyServer(
    function(input, output, session) {
        #output$myImage <- renderImage({
        #    input$tabs
        #    list(src = "plot1.png",
        #         contentType = 'image/png',
        #         width = 840,
        #         height = 840,
        #         alt = "plot")
        #}, deleteFile = FALSE)
        observe({
            occ1990 <- input$occ1990
            if (occ1990){
                #occs <<- occ_codes90
                updateSelectInput(session, "occ", "Occupations 1990",
                                  choices = occ_codes90$name,
                                  selected = occ_select90)
            }
            else{
                #occs <<- occ_codes90
                updateSelectInput(session, "occ", "Occupations",
                                  choices = occ_codes$name,
                                  selected = occ_select)
            }
        })
        output$myUsage <- renderUI({
            includeHTML("acs90.htm")
        })
        output$myggMap <- renderPlot({
            if (input$geo == "STATE"){
                stdata <- get_urbn_map(map = "states", sf = TRUE)
                stdata$horate <- NA
                indata <- yy
                #print(stdata)
                #print(statedata)
                
                for (i in 1:dim(indata)[1]){
                    if (!is.na(indata$STATE[i])){
                        stdata$horate[stdata$state_abbv == indata$STATE[i]] <- indata[i,input$sortn]/100
                    }
                }
                if (!input$incstate){
                    stdata <- stdata[!is.na(stdata$horate),]
                }
                xtitle <- paste0(as.character(input$occ),", ",
                                 input$group[1]," = ",
                                 colnames(indata)[input$sortn],", ",
                                 input$maxyear)
                xsubtitle <- paste0("(", tolower(input$units), " in ",
                                    ifelse(input$STATE != "", input$STATE, "U.S."), " states with ",
                                    input$mincount, " or more workers)")
                xfill <- paste0(colnames(indata)[input$sortn]," ",
                                ifelse(input$units != "Percent", tolower(input$units), "%"))
                mapcolors <- unlist(strsplit(input$mapcolors, ","))
                
                gg <- stdata %>% 
                    left_join(statedata, by = "state_name") %>% 
                    #ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
                    ggplot() +
                    geom_sf(mapping = aes(fill = stdata$horate),
                            color = "#ffffff", size = 0.25) +
                    #scale_fill_gradientn(labels = scales::percent) +
                    scale_fill_gradientn(labels = scales::percent,
                                         colors = mapcolors,
                                         na.value = "white",
                                         #colors = c("white",rev(rainbow(6))),
                                         guide = guide_colorbar(title.position = "top")) +
                    #labs(fill = "Homeownership rate") +
                    coord_sf(datum = NA) +
                    #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
                    theme(legend.title = element_text(),
                          legend.key.width = unit(.5, "in")) +
                    labs(fill = xfill,
                         title = xtitle,
                         subtitle = xsubtitle)
                    #xlab("longitude") +
                    #ylab("latitude")
                print(gg)
            }
            else if (input$geo == "COUNTY"){
                #xvar <- rowvar
                #gvar <- input$group[1]
                
                st_cities <- read.csv("us_cities.csv")
                if (input$STATE != ""){
                    istate <- ff$STATE[ff$STUSAB == input$STATE]
                    sstate <- sprintf("%02d", istate)
                    st_cities<-subset(st_cities,country.etc==input$STATE)
                    stdata <- urbnmapr::countydata[startsWith(urbnmapr::countydata$county_fips, sstate),]
                }
                else{
                    stdata <- urbnmapr::countydata
                }
                stdata$horate <- NA
                indata <- yy
                #indata <<- indata #DEBUG-REMOVE
                #indata$county_fips <- sprintf("%05d", indata$STCOUNTY)
                
                for (i in 1:dim(indata)[1]){
                    if (!is.na(indata$COUNTY[i])){
                        cfip <- sprintf("%05d", cc$STCOUNTY[cc$COUNTY == as.character(indata$COUNTY[i])])
                        stdata$horate[stdata$county_fips == cfip] <- indata[i,input$sortn]/100
                        #stdata$horate[stdata$county_fips == indata$county_fips[i]] <- indata[i,input$sortn]/100
                    }
                }
                if (!input$incstate){
                    stdata <- stdata[!is.na(stdata$horate),]
                }
                xtitle <- paste0(as.character(input$occ),", ",
                                 input$group[1]," = ",
                                 colnames(indata)[input$sortn],", ",
                                 input$maxyear)
                xsubtitle <- paste0("(", tolower(input$units), " in ",
                                    ifelse(input$STATE != "", input$STATE, "U.S."), " counties with ",
                                    input$mincount, " or more workers)")
                xfill <- paste0(colnames(indata)[input$sortn]," ",
                                ifelse(input$units != "Percent", tolower(input$units), "%"))
                mapcolors <- unlist(strsplit(input$mapcolors, ","))
                
                #x11(width = 8, height = 8)
                
                gg <- stdata %>% 
                    left_join(counties, by = "county_fips") %>% 
                    ##filter(state_name =="Texas") %>% 
                    #ggplot(mapping = aes(long, lat, group = group, fill = Non.ctzn)) +
                    #gg <- ggplot(data = ff, mapping = aes(long, lat, group = group, fill = horate)) +
                    ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
                    #scale_fill_gradient(low="green",high="darkgreen") +
                    #scale_fill_continuous() +
                    #ggplot(mapping = aes(long, lat, group = group, fill = as.factor(horate))) +
                    #ggplot(mapping = aes(long, lat, fill = horate)) +
                    geom_polygon(color = "#ffffff", size = .25) +
                    #geom_point(data = tx_cities, aes(long, lat), alpha = 0.5, group = tx_cities) +
                    #geom_text(data = tx_cities, aes(long, lat,label=name), alpha = 0.5) +
                    scale_fill_gradientn(labels = scales::percent,
                                         colors = mapcolors,
                                         na.value = "white",
                                         #colors = c("white",rev(rainbow(6))),
                                         guide = guide_colorbar(title.position = "top")) +
                    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
                    theme(legend.title = element_text(),
                          legend.key.width = unit(.5, "in")) +
                    labs(fill = xfill,
                         title = xtitle,
                         subtitle = xsubtitle) +
                    xlab("longitude") +
                    ylab("latitude")
                #theme_urban_map()
                skipcity <- unlist(strsplit(input$skipcity, ","))
                showcity <- unlist(strsplit(input$showcity, ","))
                if (input$STATE != ""){
                    for (i in 1:dim(st_cities)[1]){
                        city <- substr(st_cities$name[i], 1, nchar(as.character(st_cities$name[i]))-3)
                        if (city %in% skipcity) next
                        if (city %in% showcity | st_cities$pop[i] > input$minpop){
                            gg <- gg + annotate(geom = "point", x = st_cities$long[i], y = st_cities$lat[i])
                            gg <- gg + annotate(geom = "text", x = st_cities$long[i] + input$longoff,
                                                y = st_cities$lat[i], label = city)
                        }
                    }
                }
                print(gg)                
            }
            else{
                text <- "Set Geography to STATE or COUNTY (on Output tab) in order to generate a map."
                ggplot() + 
                    #annotate("text", x = 4, y = 25, size=8, label = text) + 
                    annotate("text", x = 0, y = 0, size=8, label = text) + 
                    theme_bw() +
                    theme(panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank())
            }
        }, height = 900, width = 1200)
        output$myggPlot <- renderPlot({
            if (exists("gyy") & !input$reload){
                yy <- gyy
            }
            else{
                yy <- myTable()
                gyy <<- yy
            }
            mm <- melt(yy, id=colnames(yy)[1:3])
            #print(mm) #DEBUG
            minyear <- input$minyear
            maxyear <- input$maxyear
            geovar <- "Metro"
            if (input$geo == "STATE"){
                geovar <- "State"
            }
            else if (input$geo == "COUNTY"){
                geovar <- "County"
            }
            else if (input$geo == "COUNTY_FIPS"){
                geovar <- "Fips"
            }
            else if (input$geo == "NATION"){
                geovar <- "Nation"
            }
            rowvar <- geovar
            if (input$pxremove != ""){
                up_rowvar <- str_to_upper(rowvar)
                mm[[up_rowvar]] <- gsub(input$pxremove, "", mm[[up_rowvar]])
            }
            if (minyear < maxyear | geovar == "Nation"){
                rowvar <- "Year"
            }
            colnames(mm) <- c("Year",geovar,"Count",input$group[1],"Counts")
            mm[,2] <- substr(mm[,2],1,input$geowidth)
            # if (input$geomtype == "Line Graph"){
            #     gg <- ggplot(data=mm, aes_string(x="Legend",y="Counts",group=xvar,colour=xvar,shape=xvar)) +
            #             geom_point(size=3) + xlab(input$group)
            #     if (input$linetype) gg <- gg + geom_line(aes_string(linetype=xvar), size=1)
            #     else gg <- gg + geom_line(size=1)
            # }
            if (input$geomtype == "Line Graph"){
                xvar <- rowvar
                gvar <- input$group[1]
                #gmm <<- mm
                if (input$cmin > 0 | input$cmax > 0){
                    mm <- mm[as.numeric(mm[[gvar]]) >= input$cmin & as.numeric(mm[[gvar]]) <= input$cmax,]
                }
                #if (input$cmax > 0){
                #    mm <- mm[as.numeric(mm[[gvar]]) <= input$cmax,]
                #}
                if (xvar == "Year" & input$xnum){
                    mm$Year <- as.numeric(mm$Year)
                    gg <- ggplot(data=mm, aes_string(x=xvar,y="Counts",group=gvar,colour=gvar,shape=gvar)) +
                        geom_point(size=3) + xlab(xvar) # change rowvar to xvar
                    if (input$linetype) gg <- gg + geom_line(aes_string(linetype=gvar), size=1)
                    else gg <- gg + geom_line(size=1)
                    gg <- gg + scale_x_continuous() # ADDTEST
                    #gg <- gg + scale_x_continuous(breaks=c(1990,2000,2010,2020))
                }
                else{
                    gg <- ggplot(data=mm, aes_string(x=xvar,y="Counts",group=gvar,colour=gvar,shape=gvar)) +
                        geom_point(size=3) + xlab(xvar) # change rowvar to xvar
                    if (input$linetype) gg <- gg + geom_line(aes_string(linetype=gvar), size=1)
                    else gg <- gg + geom_line(size=1)
                }
            }
            else{ # Bar Plot
                xvar <- rowvar
                gvar <- input$group[1]
                gg <- ggplot(data=mm, aes_string(x=xvar,y="Counts",fill=gvar)) +
                    geom_bar(stat="identity", position=position_dodge())
            }
            #gg <- ggplot(data=mm, aes(x=Year,y=value,fill=variable)) +
            #    geom_bar(stat="identity", position=position_dodge(),ylab="Percent")
            group1 <- input$group[1]
            g1 <- get(tolower(group1))
            if(input$color != "" & input$geomtype != "Map"){
                gg <- gg + ggtitle(myTitle())
                gg <- gg + ylab(myYlabel())
                vcolor <- unlist(strsplit(input$color, ","))
                if (length(vcolor) > 1){
                    vcolor <- rep(vcolor, length.out=NCOL(yy)-3) #DEBUG
                    gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                    gg <- gg + scale_color_manual(values = vcolor) # Line Graph
                }
                else{
                    colorCount = NCOL(yy)-3
                    if (colorCount > 12){
                        getPalette = colorRampPalette(brewer.pal(
                            brewer.pal.info[vcolor[[1]],]$maxcolors, vcolor[[1]]))
                        gg <- gg + scale_colour_manual(values = getPalette(colorCount)) # TEST
                        gg <- gg + scale_fill_manual(values = getPalette(colorCount))
                    }
                    else{
                        gg <- gg + scale_colour_brewer(palette = vcolor[[1]]) #TEST
                        gg <- gg + scale_fill_brewer(palette = vcolor[[1]])
                    }
                }
            }
            if (input$xmin < input$xmax){
                gg <- gg + coord_cartesian(xlim=c(input$xmin, input$xmax))
                if (input$xstp > 0){
                    gg <- gg + scale_x_continuous(breaks = seq(input$xmin, input$xmax, input$xstp), minor_breaks = NULL)
                }
            }
            if (input$ymin < input$ymax){
                gg <- gg + coord_cartesian(ylim=c(input$ymin, input$ymax))
                if (input$ystp > 0){
                    gg <- gg + scale_y_continuous(breaks = seq(input$ymin, input$ymax, input$ystp), minor_breaks = NULL)
                }
            }
            gg
        })
        output$myText <- renderPrint({
            if (exists("gyy") & !input$reload){
                yy <- gyy
            }
            else{
                yy <- myTable()
                gyy <<- yy
            }
            #cat(file=stderr(), "Locale =", Sys.getlocale(), "\n")
            Sys.setlocale(category = "LC_ALL", locale = "C")
            #print(paste0("Sort by ", input$xsort, ", ", input$sortdir))
            #cat(file=stderr(), "Sort by ", input$xsort, ", ", input$sortdir, "\n")
            cat(yrinfo, sep = "")
            cat("\n")
            minyear <- input$minyear
            maxyear <- input$maxyear
            if (minyear < maxyear){
                yearstr <- paste0(minyear,"-",maxyear)
            }
            else{
                yearstr <- paste0(minyear,"")
            }
            cat(paste0("AMERICAN COMMUNITIES SURVEY: ", yearstr,"\n"))
            
            if (nchar(sfilter) > 0){
                cat(paste0("(", sfilter,")\n"))
                #cat(file=stderr(), "Search (", sfilter,")\n")
            }
            cat("\n")
            ylab <- myYlabel()
            ylab <- paste0(tolower(substring(ylab,1,1)),substring(ylab,2))
            cat(paste0(myTitle()," (",ylab,")\n"))
            cat("\n")
            
            # Limit to Maximum Total Rows if necessary
            itotrows <- as.integer(input$totrows)
            if (nrow(yy) > itotrows) yy <- head(yy, n = itotrows)
            
            # Number all rows and set total width to Maximum Total Width
            if (nrow(yy) > 0) row.names(yy) <- 1:nrow(yy)
            options(width = input$totwidth)
            
            # Limit selected columns to colwith
            #if ("EMPLOYER_NAME" %in% colnames(xx)){
            #    xx$EMPLOYER_NAME <- strtrim(xx$EMPLOYER_NAME, width=input$colwidth)
            #}
            #xx$WORKSITE_CITY <- strtrim(xx$WORKSITE_CITY, width=input$colwidth)
            # Change header for selected columns
            colnames(yy)[colnames(yy)=="year"]  <- "Year"
            colnames(yy)[colnames(yy)=="count"] <- "Count"
            
            dp <- input$decplaces
            if (input$units == "Count"){
                #print(str(yy)) #DEBUG-REMOVE
                for (i in 3:NCOL(yy)){
                    yy[,i] <- format(yy[,i], big.mark=",", scientific=FALSE)
                }
                
            }
            else if (input$units %in% c("Mean Wage in 1000s of $","Mean Wage in 1000s of 2018$")){
                #print(str(yy)) #DEBUG-REMOVE
                for (i in 3:NCOL(yy)){
                    #yy[,i] <- format(yy[,i], big.mark=",", scientific=FALSE)
                    yy[,i] <- format(round(yy[,i], dp), big.mark=",", scientific=FALSE)
                }
                
            }
            else{
                yy[,3] <- format(yy[,3], big.mark=",", scientific=FALSE)
                #print(str(yy)) #DEBUG-REMOVE
                for (i in 4:NCOL(yy)){
                    yy[,i] <- format(round(yy[,i], dp), nsmall = dp)
                }
            }
            if (input$geo == "NATION"){
                yy <- yy[,c(1,3:NCOL(yy))] # remove duplicate YEAR
            }
            print(yy)
            cat("\nURL parameters (short)=\n")
            urlshort <- print_url(input, session, inputs, FALSE)
            cat(urlshort)
            if (urlshort != "") cat("\n")
            cat("\nURL parameters (long)=\n")
            cat(paste0(print_url(input, session, inputs, TRUE),"\n"))
        })
        saveCSV <- observeEvent(input$savecsv,{
            if (input$csvname != ""){
                yy <- myTable()
                if (input$geo == "NATION"){
                    yy <- yy[,c(1,3:NCOL(yy))] # remove duplicate YEAR
                }
                write_csv(yy, paste0(input$csvname,".csv"))
            }
        }, ignoreNULL = FALSE)
        myTitle <- reactive({
            minyear <- input$minyear
            maxyear <- input$maxyear
            if (minyear < maxyear){
                yearstr <- paste0(minyear,"-",maxyear)
            }
            else{
                yearstr <- paste0(minyear,"")
            }
            group <- input$group
            groupstr <- ""
            if (length(group) == 1){
                groupstr <- paste0("grouped by ",input$group[1])
            }
            else if (length(group) > 1){
                groupstr <- paste0("grouped by ",input$group[1]," and ",input$group[2])
            }
            if (minyear < maxyear){
                if (input$STATE != ""){
                    groupstr <- paste0(groupstr,", STATE=", input$STATE)
                }
                if (input$COUNTY != ""){
                    groupstr <- paste0(groupstr,", COUNTY=", input$COUNTY)
                }
                if (input$METRO != ""){
                    groupstr <- paste0(groupstr,", METRO=", input$METRO)
                }
            }
            ptitle <- input$ptitle
            nc <- nchar(ptitle)
            if (nc == 0){
                title <- paste0(input$occ,": ",yearstr,", ",groupstr)
            }
            else if (nc > 1 & substring(ptitle,nc-1) == "=="){
                title <- paste0(substring(ptitle,0,nc-2)," ",input$occ,": ",yearstr,", ",groupstr)
            }
            else if (nc > 1 & substring(ptitle,nc-1) == "++"){
                title <- paste0(substring(ptitle,0,nc-2)," ",yearstr,", ",groupstr)
            }
            else{
                title <- ptitle
            }
        })
        myYlabel <- reactive({
            ylabel <- input$units
            if (input$units == "Percent in group"){
                ylabel <- paste0("Percent in ",input$group[1]," group")
            }
            ylabel
        })
        myTable <- reactive({
            inputs <<- read.csv("inputs.csv", stringsAsFactors = FALSE) 
            check_url(input, session, inputs)
            
            # Initialize any local variables
            xsearch <- c("STATE","COUNTY","METRO")
            
            # Load statefip to fip dataframe
            #fipref <- "https://www2.census.gov/geo/docs/reference/state.txt"
            fipref <- "state.txt" # read from local memory
            #header=STATE|STUSAB|STATE_NAME|STATENS
            ff <<- read.csv(fipref, sep = "|")
            fips <- data.frame(ff$STATE, ff$STUSAB)
            colnames(fips) <- c("STATEFIP", "STATE")
            fips <<- fips # make visible for debug
            
            # Load county fip to county dataframe
            #countyref = "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt"
            countyref = "national_county.txt" # read from local memory
            cc <- read.csv(countyref, header = FALSE)
            colnames(cc) <- c("STATE","STATEFIP","COUNTYFIP","COUNTY","H1")
            cc$STCOUNTY <- cc$STATEFIP * 1000 + cc$COUNTYFIP
            cc$COUNTY <- do.call(paste, cc[c("COUNTY", "STATE")])
            cc <<- cc
            counties <- data.frame(cc$STCOUNTY, cc$COUNTY)
            colnames(counties) <- c("STCOUNTY", "COUNTY")
            #counties <<- counties # make visible for debug
            
            # Load metro fip to metro dataframe
            metref = "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2017/delineation-files/list2.xls"
            filepath <- "list2.xls"
            if (!file.exists(filepath)) {
                download.file(metref, filepath, mode = "wb")
            }
            metrox <- read_excel(filepath, col_names = FALSE, skip = 8)
            metrox1 <<- metrox #DEBUG
            metrox <- aggregate(metrox, by=list(metrox$...1), FUN=min)
            metrox2 <<- metrox #DEBUG
            metro <- data.frame(metrox$...1, metrox$...2)
            colnames(metro) <- c("MET2013", "METRO")
            metro <<- metro # make visible for debug
            
            # Load variable code dataframes
            citizenship <<- read.csv("citizenship.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(citizenship) <<- c("column","min","max","label")
            citizenship2 <<- read.csv("citizenship2.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(citizenship2) <<- c("column","min","max","label")
            ctznshp <<- read.csv("ctznshp.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(ctznshp) <<- c("column","min","max","label")
            educ <<- read.csv("educ.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(educ) <<- c("column","min","max","label")
            education <<- read.csv("education.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(education) <<- c("column","min","max","label")
            masters <<- read.csv("masters.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(masters) <<- c("column","min","max","label")
            emps <<- read.csv("emps.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(emps) <<- c("column","min","max","label")
            empstat <<- read.csv("empstat.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(empstat) <<- c("column","min","max","label")
            sex <<- read.csv("sex.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(sex) <<- c("column","min","max","label")
            sex_m_f <<- read.csv("sex_m_f.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(sex_m_f) <<- c("column","min","max","label")
            age5 <<- read.csv("age5.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(age5) <<- c("column","min","max","label")
            age10 <<- read.csv("age10.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(age10) <<- c("column","min","max","label")
            incwage20 <<- read.csv("incwage20.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(incwage20) <<- c("column","min","max","label")
            wkswork2 <<- read.csv("wkswork2.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(wkswork2) <<- c("column","min","max","label")
            birthplace <<- read.csv("birthplace.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(birthplace) <<- c("column","min","max","label")
            birthplace2 <<- read.csv("birthplace2.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(birthplace2) <<- c("column","min","max","label")
            birthplace_tech <<- read.csv("birthplace_tech.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(birthplace_tech) <<- c("column","min","max","label")
            race <<- read.csv("race.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(race) <<- c("column","min","max","label")
            race4 <<- read.csv("race4.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(race4) <<- c("column","min","max","label")
            asian <<- read.csv("asian.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(asian) <<- c("column","min","max","label")
            hispan <<- read.csv("hispan.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(hispan) <<- c("column","min","max","label")
            occ <<- read.csv("occ.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(occ) <<- c("column","min","max","label")
            occ_cis <<- read.csv("occ_cis.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(occ_cis) <<- c("column","min","max","label")
            occ1990 <<- read.csv("occ1990.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(occ1990) <<- c("column","min","max","label")
            degfield <<- read.csv("degfield.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(degfield) <<- c("column","min","max","label")
            degfield2 <<- read.csv("degfield2.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(degfield2) <<- c("column","min","max","label")
            poverty200 <<- read.csv("poverty200.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(poverty200) <<- c("column","min","max","label")
            poverty300 <<- read.csv("poverty300.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(poverty300) <<- c("column","min","max","label")
            hisprace <<- read.csv("hisprace.csv", header = FALSE, stringsAsFactors = FALSE)
            colnames(hisprace) <<- c("column","min","max","label")
            
            yy <- NULL
            yrinfo <- NULL
            minyear  <- input$minyear
            maxyear  <- input$maxyear
            stepyear <- input$stepyear
            if (maxyear < minyear) maxyear <- minyear
            #for (year in minyear:maxyear){
            maxyear2 <- floor((maxyear-minyear)/stepyear)*stepyear + minyear
            seqyear <- seq(maxyear2,minyear,-stepyear)
            if (maxyear != maxyear2){
                seqyear <- c(maxyear, seqyear)
            }
            for (year in seqyear){
                #if(!(year %in% c(1980,1990,2000,2002,2004,2005,2006,2008,2010,2012,2014,2016,2017,2018))) next
                yr <- year %% 100
                # Read csv file if necessary
                ooyear <- paste0("oo",year)
                if (!exists(ooyear)){
                    #csvfile  <- paste0("usa",year,"p90stem.csv") #  OLD - set to fit data on shinyapps.io
                    csvfile  <- paste0("usa",year,"p90.csv") # OLD - use full data for 2010 and 2017
                    csvfilew  <- paste0("usa",year,"p90w.csv") # full data file
                    if (!file.exists(csvfile)){
                        if (!file.exists(csvfilew)){
                            next
                        }
                        else{
                            if (year >= 2020){
                                oo <- read.csv(csvfilew, colClasses=
                                    # Year,     Sample,   Serial,   Cbserial, Hhwt,   Cluster
                                    c(NA,       "NULL",   "NULL",   "NULL",   "NULL", "NULL",
                                    # Statefip, Countyfip,Met2013,  Puma,     Strata, GQ
                                      NA,       NA,       NA,       "NULL",   "NULL", "NULL",
                                    # Pernum,   Perwt,    Sex,      Age,      Race,
                                      "NULL",   NA,       NA,       NA,       NA,
                                    # Raced,    Hispan,   Hispand,  Bpl,      Bpld,
                                      "NULL",   NA,       "NULL",   NA,       "NULL",
                                    # Citizen,  Yrimmig,  Educ,     Educd,
                                      NA,       "NULL",   "NULL",   NA,
                                    # DegField, Degfieldd,DegField2,DegField2d,Empstat,
                                      NA,       "NULL",   NA,       "NULL",    NA,
                                    # Empstatd, Occ,      Classwkr, Classwkrd
                                    #  "NULL",   NA,       "NULL",   "NULL",
                                    # Empstatd,  Classwkr, Classwkrd, Occ,      Occ1990
                                      "NULL",       "NULL",   "NULL",   NA,       NA,
                                    # Wkswork2, IncWage,  Poverty
                                      NA,       NA,       NA))
                                #oo <- oo[oo$OCC1990 < 400,] # add to fit data on shinyapps.io
                                oo$HISPRACE <- oo$RACE
                                oo$HISPRACE[oo$HISPRACE == 0] <- 99
                                oo$HISPRACE[oo$HISPAN != 0] <- 0
                            }
                            else if (year == 2019){
                                oo <- read.csv(csvfilew, colClasses=
                                    # Year,     Sample,   Serial,   Cbserial, Hhwt,   Cluster
                                    c(NA,       "NULL",   "NULL",   "NULL",   "NULL", "NULL",
                                    # Statefip, Countyfip,Met2013,  Puma,     Strata, GQ
                                      NA,       NA,       NA,       "NULL",   "NULL", "NULL",
                                    # Pernum,   Perwt,    Sex,      Age,      Race,
                                      "NULL",   NA,       NA,       NA,       NA,
                                    # Raced,    Hispan,   Hispand,  Bpl,      Bpld,
                                      "NULL",   NA,       "NULL",   NA,       "NULL",
                                    # Citizen,  Yrimmig,  PredHisp, Educ,     Educd,
                                      NA,       "NULL",   NA,       "NULL",   NA,
                                    # DegField, Degfieldd,DegField2,DegField2d,Empstat,
                                      NA,       "NULL",   NA,       "NULL",    NA,
                                    # Empstatd, Occ,      Classwkr, Classwkrd
                                    #  "NULL",   NA,       "NULL",   "NULL",
                                    # Empstatd,  Classwkr, Classwkrd, Occ,      Occ1990
                                      "NULL",       "NULL",   "NULL",   NA,       NA,
                                    # Wkswork2, IncWage,  Poverty
                                      NA,       NA,       NA))
                                #oo <- oo[oo$OCC1990 < 400,] # add to fit data on shinyapps.io
                                oo$HISPRACE <- oo$RACE
                                oo$HISPRACE[oo$HISPRACE == 0] <- 99
                                #oo$HISPRACE[oo$PREDHISP != 0] <- 10
                                oo$HISPRACE[oo$HISPAN != 0] <- 0 # use instead of PREDHISP
                            }
                            else if (year == 2018){
                                oo <- read.csv(csvfilew, colClasses=
                                    # Year,     Sample,   Serial,   Cbserial, Hhwt,   Cluster
                                    c(NA,       "NULL",   "NULL",   "NULL",   "NULL", "NULL",
                                    # Statefip, Countyfip,Met2013,  Puma,     Stata,  GQ
                                      NA,       NA,       NA,       "NULL",   "NULL", "NULL",
                                    # Pernum,   Perwt,    Sex,      Age,      Race,
                                      "NULL",   NA,       NA,       NA,       NA,
                                    # Raced,    Hispan,   Hispand,  Bpl,      Bpld,
                                      "NULL",   NA,       "NULL",   NA,       "NULL",
                                    # Citizen,  Yrimmig,  Educ,     Educd,
                                      NA,       "NULL",   "NULL",   NA,
                                    # DegField, Degfieldd,DegField2,DegField2d,Empstat,
                                      NA,       "NULL",   NA,       "NULL",    NA,
                                    # Empstatd, Occ,      Classwkr, Classwkrd
                                    #  "NULL",   NA,       "NULL",   "NULL",
                                    # Empstatd, Occ,      Occ1990,  Classwkr, Classwkrd
                                      "NULL",   NA,       NA,       "NULL",   "NULL",
                                    # Wkswork2, IncWage
                                      NA,       NA))
                                #oo <- oo[oo$OCC1990 < 400,] # add to fit data on shinyapps.io
                            }
                            else if (year >= 2009){
                                oo <- read.csv(csvfilew, colClasses=
                                    # Year,     Datanum,  Serial,   Cbserial, Hhwt
                                    #  c(NA,       "NULL",   "NULL",   "NULL",   "NULL",
                                    # Year,     Datanum,  Serial,   Cbserial, Hhwt      Cluster
                                    c(NA,       "NULL",   "NULL",   "NULL",   "NULL",   "NULL",
                                    # Statefip, Countyfip,Met2013,  Puma,     GQ
                                    #  NA,       NA,       NA,       "NULL",   "NULL",
                                    # Statefip, Countyfip,Met2013,  Puma,     STRATA,   GQ
                                      NA,       NA,       NA,       "NULL",   "NULL",   "NULL",
                                    # Pernum,   Perwt,    Sex,      Age,      Race,
                                      "NULL",   NA,       NA,       NA,       NA,
                                    # Raced,    Hispan,   Hispand,  Bpl,      Bpld,
                                      "NULL",   NA,       "NULL",   NA,       "NULL",
                                    # Citizen,  Yrimmig,  Educ,     Educd,
                                      NA,       "NULL",   "NULL",   NA,
                                    # DegField, Degfieldd,DegField2,DegField2d,Empstat,
                                      NA,       "NULL",   NA,       "NULL",    NA,
                                    # Empstatd, Occ,      Occ1990,  Classwkr, Classwkrd
                                      "NULL",   NA,       NA,       "NULL",   "NULL",
                                    # Empstatd, Occ,                Classwkr, Classwkrd
                                    #  "NULL",   NA,                 "NULL",   "NULL",
                                    # Wkswork2, IncWage
                                    NA,       NA))
                                #oo <- oo[oo$OCC1990 < 400,] # add to fit data on shinyapps.io
                            }
                            else if (year == 2010){
                                oo <- read.csv(csvfilew, colClasses=
                                    # Year,     Datanum,  Serial,   Cbserial, Hhwt
                                    c(NA,       "NULL",   "NULL",   "NULL",   "NULL",
                                    # Statefip, Countyfip,Met2013,  Puma,     GQ
                                      NA,       NA,       NA,       "NULL",   "NULL",
                                    # Pernum,   Perwt,    Sex,      Age,      Race,
                                      "NULL",   NA,       NA,       NA,       NA,
                                    # Raced,    Hispan,   Hispand,  Bpl,      Bpld,
                                      "NULL",   NA,       "NULL",   NA,       "NULL",
                                    # Citizen,  Yrimmig,  Educ,     Educd,    Empstat,    
                                      NA,       "NULL",   "NULL",   NA,       NA,
                                    # Empstatd, Occ,      Occ1990,  Classwkr, Classwkrd
                                      "NULL",   NA,       NA,       "NULL",   "NULL",
                                    # Wkswork2, IncWage
                                      NA,       NA))
                                #oo <- oo[oo$OCC1990 < 400,] # add to fit data on shinyapps.io
                            }
                            else if (year >= 2001){
                                oo <- read.csv(csvfilew, colClasses=
                                    # Year,     Sample,   Serial,   Cbserial, Hhwt,   Cluster
                                    c(NA,       "NULL",   "NULL",   "NULL",   "NULL", "NULL",
                                    # Statefip, Countyfip,Met2013,  Puma,     Strata, GQ
                                      NA,       NA,       NA,       "NULL",   "NULL", "NULL",
                                    # Pernum,   Perwt,    Sex,      Age,      Race,
                                      "NULL",   NA,       NA,       NA,       NA,
                                    # Raced,    Hispan,   Hispand,  Bpl,      Bpld,
                                      "NULL",   NA,       "NULL",   NA,       "NULL",
                                    # Citizen,  Yrimmig,  Educ,     Educd,    Empstat,    
                                      NA,       "NULL",   "NULL",   NA,       NA,
                                    # Empstatd, Occ,      Occ1990,  Classwkr, Classwkrd
                                     "NULL",   NA,       NA,       "NULL",   "NULL",
                                    # Wkswork2, IncWage
                                      NA,       NA))
                                #oo <- oo[oo$OCC1990 < 400,] # add to fit data on shinyapps.io
                            }
                            else if (year == 2000){
                                # missing Cbserial
                                oo <- read.csv(csvfilew, colClasses=
                                    # Year,     Datanum,  Serial,   Cbserial, Hhwt
                                    c(NA,       "NULL",   "NULL",             "NULL",
                                    # Statefip, Countyfip,Met2013,  Puma,     GQ
                                      NA,       NA,       NA,       "NULL",   "NULL",
                                    # Pernum,   Perwt,    Sex,      Age,      Race,
                                      "NULL",   NA,       NA,       NA,       NA,
                                    # Raced,    Hispan,   Hispand,  Bpl,      Bpld,
                                      "NULL",   NA,       "NULL",   NA,       "NULL",
                                    # Citizen,  Yrimmig,  Educ,     Educd,    Empstat,    
                                      NA,       "NULL",   "NULL",   NA,       NA,
                                    # Empstatd, Occ,      Occ1990,  Classwkr, Classwkrd
                                      "NULL",   NA,       NA,       "NULL",   "NULL",
                                    # Wkswork2, IncWage
                                      NA,       NA))
                                #oo <- oo[oo$OCC1990 < 400,]
                            }
                            else if (year == 1990){
                                oo <- read.csv(csvfilew, colClasses=
                                    # Year,     Datanum,  Serial,   Cbserial, Hhwt
                                    c(NA,       "NULL",   "NULL",             "NULL",
                                    # Statefip, Countyfip,Met2013,  Puma,     GQ
                                      NA,       NA,                 "NULL",   "NULL",
                                    # Pernum,   Perwt,    Sex,      Age,      Race,
                                      "NULL",   NA,       NA,       NA,       NA,
                                    # Raced,    Hispan,   Hispand,  Bpl,      Bpld,
                                      "NULL",   NA,       "NULL",   NA,       "NULL",
                                    # Citizen,  Yrimmig,  Educ,     Educd,    Empstat,    
                                      NA,       "NULL",   "NULL",   NA,       NA,
                                    # Empstatd, Occ,      Occ1990,  Classwkr, Classwkrd
                                      "NULL",   NA,       NA,       "NULL",   "NULL",
                                    # Wkswork2, IncWage
                                      NA,       NA))
                                #oo <- oo[oo$OCC1990 < 400,]
                            }
                            else{
                                oo <- read.csv(csvfilew, colClasses=
                                    # Year,     Datanum,  Serial,   Cbserial, Hhwt
                                    c(NA,       "NULL",   "NULL",             "NULL",
                                    # Statefip, Countyfip,Met2013,  Puma,     GQ
                                      NA,       NA,                           "NULL",
                                    # Pernum,   Perwt,    Sex,      Age,      Race,
                                      "NULL",   NA,       NA,       NA,       NA,
                                    # Raced,    Hispan,   Hispand,  Bpl,      Bpld,
                                      "NULL",   NA,       "NULL",   NA,       "NULL",
                                    # Citizen,  Yrimmig,  Educ,     Educd,    Empstat,    
                                      NA,       "NULL",   "NULL",   NA,       NA,
                                    # Empstatd, Occ,      Occ1990,  Classwkr, Classwkrd
                                      "NULL",   NA,       NA,       "NULL",   "NULL",
                                    # Wkswork2, IncWage
                                      NA,       NA))
                                #if (year <= 2000){
                                #    oo <- oo[oo$OCC1990 < 400,]
                                #}
                            }
                            write.csv(oo, csvfile)
                        }
                    }
                    if (!file.exists(csvfile)){
                        print(paste("File", csvfile, "missing"))
                    }
                    else{
                        #print(paste("READ", csvfile))
                        msg = paste("Loading CSV data for FY", year)
                        withProgress(message = msg, detail = "this can take a minute or so...", value = 0, {
                            for (i in 1:9){
                                incProgress(1/10)
                                Sys.sleep(0.5)
                            }
                            if (in_shinyapps){
                                oo <- read.csv(csvfile, fileEncoding="latin1")
                                #assign(ooyear, read.csv(csvfile, fileEncoding="latin1"), inherits = TRUE)
                            }
                            else {
                                oo <- read.csv(csvfile)
                                #assign(ooyear, read.csv(csvfile), inherits = TRUE)
                            }
                            if (year >= 2018){
                                oo$OCC1990 <- NA
                                oo$OCC1990[oo$OCC == 700] <- 65
                                oo$OCC1990[oo$OCC >= 1000 & oo$OCC <= 1009] <- 64
                                oo$OCC1990[oo$OCC >= 1010 & oo$OCC <= 1029] <- 229
                                oo$OCC1990[oo$OCC >= 1030 & oo$OCC <= 1199] <- 64
                                oo$OCC1990[oo$OCC >= 1200 & oo$OCC <= 1219] <- 66
                                oo$OCC1990[oo$OCC >= 1220 & oo$OCC <= 1239] <- 65
                                oo$OCC1990[oo$OCC >= 1240 & oo$OCC <= 1299] <- 68
                            }
                            assign(ooyear, oo, inherits = TRUE)
                            incProgress(1/10)
                        })
                    }
                }
                xx <- data.frame(get(ooyear))
                ##COUNTYFIP SHOULD BE THERE ALREADY #DEBUG
                ##cloc <- which(names(xx) == "COUNTY")
                ##if (length(cloc) == 0) cloc <- which(names(xx) == "COUNTYFIP")
                ##colnames(xx)[cloc] <- "COUNTYFIP"
                mloc <- which(names(xx) == "CBSERIAL")
                if (length(mloc) == 0) xx$CBSERIAL <- 0
                mloc <- which(names(xx) == "MET2013")
                if (length(mloc) == 0) xx$MET2013 <- 0
                if (input$empstat != "All"){
                    if (input$empstat == "Employed") xx <- xx[xx$EMPSTAT == 1,]
                    else if (input$empstat == "Unemployed") xx <- xx[xx$EMPSTAT == 2,]
                    else if (input$empstat == "In labor force") xx <- xx[xx$EMPSTAT <= 2,]
                }
                if (input$xrange1 != ""){
                    #xx <- xx[xx[[input$xrange1]] >= input$minrange1 & xx[[input$xrange1]] <= input$maxrange1,]
                    xx <- xx[as.integer(xx[[input$xrange1]]) >= input$minrange1 &
                             as.integer(xx[[input$xrange1]]) <= input$maxrange1,]
                }
                if (input$xrange2 != ""){
                    #xx <- xx[xx[[input$xrange2]] >= input$minrange2 & xx[[input$xrange2]] <= input$maxrange2,]
                    xx <- xx[as.integer(xx[[input$xrange2]]) >= input$minrange2 &
                             as.integer(xx[[input$xrange2]]) <= input$maxrange2,]
                }
                #gxx <<- xx #DEBUG
                #xx <- xx[xx$OCC1990 >= occs$min[iocc] & xx$OCC1990 <= occs$max[iocc],]
                if (input$occ1990){
                    occs <<- occ_codes90
                    iocc <- which(occs$name == input$occ)
                    occ_select90 <<- input$occ
                    if (occs$name[iocc] == "Math & Computer Scientists & SW Developers (64-68,229)"){
                        xx <- xx[(xx$OCC1990 >= 64 & xx$OCC1990 < 69) | xx$OCC1990 == 229,]
                    }
                    else{
                        xx <- xx[xx$OCC1990 >= occs$min[iocc] & xx$OCC1990 < occs$max[iocc],]
                    }
                 }
                else{
                    occs <<- occ_codes
                    iocc <- which(occs$name == input$occ)
                    occ_select <<- input$occ
                    if (occs$name[iocc] == "Computer and Mathematical & Logisticians"){
                        xx <- xx[(xx$OCC >= 1000 & xx$OCC < 1300) | xx$OCC == 700,]
                    }
                    else{
                        xx <- xx[xx$OCC >= occs$min[iocc] & xx$OCC < occs$max[iocc],]
                    }
                }
                xx <- merge(xx, fips, by = "STATEFIP")
                xx$STCOUNTY <- xx$STATEFIP * 1000 + (xx$COUNTYFIP) #DEBUG - REMOVE DIVIDE BY 10 SINCE FIP IS NOW VALID
                xx <- merge(xx, counties, by = "STCOUNTY", all.x = TRUE)
                xx <- merge(xx, metro, by = "MET2013", all.x = TRUE)
                #print(dim(xx)) #DEBUG
                totcount <- sum(xx$PERWT[!is.na(xx$PERWT)])
                yritem <- paste(year, " Total count =", format(totcount, big.mark=",",scientific=FALSE), "\n")
                if (is.null(yrinfo)){
                    yrinfo <- yritem
                }
                else{
                    yrinfo <- c(yrinfo, yritem)
                }
                yrinfo <<- yrinfo
                
                #xx <<- xx # make visible for debug
                # Make value changes before searches
                
                # Do searches
                sfilter <- ""
                for (i in 1:length(xsearch)){
                    #pattern <- trimws(input[[xsearch[i]]]) # trim whitespace
                    pattern <- input[[xsearch[i]]]
                    if (nchar(pattern) > 0){
                        xx <- xx[grep(pattern, xx[[xsearch[i]]], ignore.case = TRUE),]  
                        #print(paste0("Search ", xsearch[i], " for ", pattern))
                        sfilter <- paste0(sfilter,", ", xsearch[i], "=", pattern)
                    }
                }
                sfilter <<- sub("^, ","",sfilter)
                
                ngroup <- length(input$group)
                if (ngroup > 0){
                    dd <- data.table(xx)
                    group1 <- input$group[1]
                    g1 <- get(tolower(group1))
                    dd$group1 <- ""
                    dd$group2 <- ""
                    groups <- "group1"
                    hdrwidth1 <- input$hdrwidth
                    if (ngroup > 1){
                        hdrwidth1 <- floor(input$hdrwidth / 2)
                        hdrwidth2 <- input$hdrwidth - hdrwidth1 - 1
                        groups <- "group1,group2"
                        group2 <- input$group[2]
                        g2 <- get(tolower(group2))
                        dd$group2 <- ""
                        for (i in 1:NROW(g2)){
                            dd$group2[dd[[g2$column[i]]] >= g2$min[i] & dd[[g2$column[i]]] <= g2$max[i]] <- substr(g2$label[i],1,hdrwidth2+3)
                        }
                    }
                    for (i in 1:NROW(g1)){
                        dd$group1[dd[[g1$column[i]]] >= g1$min[i] & dd[[g1$column[i]]] <= g1$max[i]] <- substr(g1$label[i],1,hdrwidth1+3)
                    }
                    if (input$geo == "COUNTY"){
                        #groups <- paste0("COUNTY,", input$group, collapse=',')
                        groups <- paste0("COUNTY,", groups, collapse=',')
                        #groups <- "COUNTY, group1"
                    }
                    else if (input$geo == "COUNTY_FIPS"){
                        #groups <- paste0("STCOUNTY,", groups, collapse=',') #TEST
                        groups <- paste0("COUNTY,", groups, collapse=',') #FIXMAP
                    }
                    else if (input$geo == "NATION"){
                        #groups <- paste0(input$geo, ",", input$group, collapse=',')
                        groups <- paste0("YEAR,", groups, collapse=',')
                        #groups <- paste0(input$geo, ", ", groups)
                    }
                    else{
                        #groups <- paste0(input$geo, ",", input$group, collapse=',')
                        groups <- paste0(input$geo, ",", groups, collapse=',')
                        #groups <- paste0(input$geo, ", ", groups)
                    }
                    #print(paste("groups=",groups))
                    #dd <<- dd #DEBUG
                    if (input$units == "Mean Wage in 1000s of $" | input$units == "Mean Wage in 1000s of 2018$"){
                        dd <- dd[!is.na(dd$INCWAGE) & !is.na(dd$PERWT),]
                        #dd$TOTWAGE <- as.integer(dd$PERWT * dd$INCWAGE)
                        #gg <- dd[, .(COUNT = sum(PERWT), APPLICATIONS = length(PERWT), TOTALS = as.integer(sum(PERWT*INCWAGE))), by = groups]
                        gg <- dd[, .(COUNT = sum(PERWT), APPLICATIONS = length(PERWT), TOTALS = sum(INCWAGE * as.double(PERWT))), by = groups]
                        #gg <- dd[, .(COUNT = sum(PERWT), APPLICATIONS = length(PERWT), TOTALS = sum(TOTWAGE)), by = groups]
                        #gg$COUNT <- gg$TOTALS / (gg$COUNT * 1000.0)
                        gg$COUNT <- gg$TOTALS / gg$COUNT
                        gg$COUNT <- gg$COUNT / 1000
                        #gg$COUNT <- gg$TOTALS
                        if (input$units == "Mean Wage in 1000s of 2018$"){
                            defl <- cpiu$Value[cpiu$Year == year] / cpiu$Value[cpiu$Year == 2018]
                            gg$COUNT <- gg$COUNT / defl
                        }
                    }
                    else{
                        gg <- dd[, .(COUNT = sum(PERWT), APPLICATIONS = length(PERWT)), by = groups]
                    }
                    gg <- gg[order(-COUNT)]
                    if (ngroup > 1){
                        if (input$geo == "METRO"){
                            gg <- cast(gg, METRO~group1+group2, value = "COUNT")
                        }
                        else if (input$geo == "COUNTY"){
                            gg <- cast(gg, COUNTY~group1+group2, value = "COUNT")
                        }
                        else if (input$geo == "COUNTY_FIPS"){
                            #gg <- cast(gg, STCOUNTY~group1+group2, value = "COUNT") #TEST
                            gg <- cast(gg, COUNTY~STCOUNTY+group1+group2, value = "COUNT") #FIXMAP
                        }
                        else if (input$geo == "STATE"){
                            gg <- cast(gg, STATE~group1+group2, value = "COUNT")
                        }
                        else{
                            gg <- cast(gg, YEAR~group1+group2, value = "COUNT")
                        }
                    }
                    else{
                        if (input$geo == "METRO"){
                            gg <- cast(gg, METRO~group1, value = "COUNT")
                        }
                        else if (input$geo == "COUNTY"){
                            gg <- cast(gg, COUNTY~group1, value = "COUNT")
                        }
                        else if (input$geo == "COUNTY_FIPS"){
                            #gg <- cast(gg, STCOUNTY~group1, value = "COUNT") #TEST
                            gg <- cast(gg, COUNTY~group1, value = "COUNT") #FIXMAP
                        }
                        else if (input$geo == "STATE"){
                            gg <- cast(gg, STATE~group1, value = "COUNT")
                        }
                        else{
                            gg <- cast(gg, YEAR~group1, value = "COUNT") 
                        }
                    }
                    gg <<- gg #DEBUG
                    gg$Year <- paste0("",year) #TESTTEST
                    #gg$Year <- year
                    gg$count <- 0
                    #gg <- gg[,c(1,NCOL(gg),2:(NCOL(gg)-1))]
                    gg <- gg[,c(NCOL(gg)-1,1,NCOL(gg),2:(NCOL(gg)-2))] # reorder columns
                    
                    if (input$units == "Percent in group"){
                        for (i in 4:NCOL(gg)){
                            colnames(gg)[i] <- gsub(".--","",colnames(gg)[i])
                        }
                        i <- 4
                        while (i < NCOL(gg)){
                            pos <- regexpr("_", colnames(gg)[i])
                            hdr1 <- substr(colnames(gg)[i], 1, pos)
                            subcount <- 0
                            i1 <- i
                            for (i in i1:NCOL(gg)){
                                i2 <- i
                                pos <- regexpr("_", colnames(gg)[i])
                                hdr2 <- substr(colnames(gg)[i], 1, pos)
                                if (hdr1 != hdr2){
                                    i2 <- i - 1
                                    break
                                }
                                gg[,i][is.na(gg[,i])] <- 0
                                gg$count <- gg$count + gg[,i]
                                subcount <- subcount + gg[,i]
                            }
                            for (i in i1:i2){
                                gg[,i] <- 100 * gg[,i] / subcount
                            }
                            i <- i2 + 1
                        }
                        gg <- gg[gg$count >= input$mincount,]
                    }
                    else{
                        for (i in 4:NCOL(gg)){
                            gg[,i][is.na(gg[,i])] <- 0
                            gg$count <- gg$count + gg[,i]
                            colnames(gg)[i] <- gsub(".--","",colnames(gg)[i])
                        }
                        gg <- gg[gg$count >= input$mincount,]
                        for (i in 4:NCOL(gg)){
                            if (input$units == "Percent"){
                                gg[,i] <- 100 * gg[,i] / gg$count
                            }
                        }
                    }
                    if (input$showcol != ""){
                        keepcols <- c(1,2,3)
                        for (i in 4:NCOL(gg)){
                            if (input$showcol != ""){
                                if (str_detect(colnames(gg)[i], input$showcol)){
                                    keepcols <- c(keepcols, i)
                                }
                            }
                        }
                        gg <- gg[,keepcols]
                        #ggg <<- gg #DEBUG
                        
                    }
                    xx <- data.frame(gg)
                    #options(width = input$totwidth)
                    #print(head(hh, n=input$totrows))
                    #gg <<- gg #DEBUG
                }
                # Sort by specified sort fields
                sortn <- input$sortn
                if (sortn == 0) sortn <- 1
                if (sortn < 0) sortn <- sortn + NCOL(xx) + 1
                if (input$sortdir == "Ascending") xx <- xx[order(xx[sortn]),]
                else{
                    #if (class(xx[[input$xsort]])=="factor") xx <- xx[rev(order(xx[input$sortn])),]
                    xx <- xx[order(-xx[sortn]),]
                }
                if (is.null(yy)){
                    yy <- xx
                }
                else{
                    yy <- rbind.fill(yy, xx)
                }
                #if (year == 1990) rm(oo1990) #DEBUG
            }
            if (maxyear > minyear){
                yy <- yy[seq(dim(yy)[1],1),] # put in ascending order
            }
            cat(file=stderr(), 
                paste0("v1: ", print_url(input, session, inputs, FALSE),"\n"))
            #yy[is.na(yy)] <- 0
            yy <<- yy #DEBUG
        })
    }
)
print_url <- function(input, session, ii, longFmt){
    parms <- ""
    nparms <- 0
    prefix <- "?"
    for (i in 1:NROW(ii)){
        if (ii$type[i] == "selectInput"){
            if (ii$other[i] != ""){
                parmstr <- paste(input[[ii$inputId[i]]], collapse = ii$other[i])
                parmstr <- paste(unlist(input[[ii$inputId[i]]]), collapse = '|')
            }
            else{
                parmstr <- input[[ii$inputId[i]]]
            }
        }
        else{
            parmstr <- input[[ii$inputId[i]]]
        }
        zzparmstr <<- parmstr #DEBUG
        zziidefault <<- ii$default[i] #DEBUG
        if ((parmstr != ii$default[i]) | longFmt){
            if (nparms > 0) prefix <- "&"
            parms <- paste0(parms,prefix,ii$inputId[i],"=",gsub(" ","%20",parmstr))
            nparms <- nparms + 1
        }
    }
    return(parms)
}
check_url <- function(input, session, ii){
    if (input$ignore == FALSE){
        query <- parseQueryString(session$clientData$url_search)
        for (i in 1:NROW(ii)){
            if (!is.null(query[[ii$inputId[i]]])){
                if (ii$type[i] == "checkboxInput"){
                    updateCheckboxInput(session, ii$inputId[i], value = as.logical(query[[ii$inputId[i]]]))
                }
                else if (ii$type[i] == "numericInput"){
                    updateNumericInput(session, ii$inputId[i], value = as.numeric(query[[ii$inputId[i]]]))
                }
                else if (ii$type[i] == "selectInput"){
                    selects <- query[[ii$inputId[i]]]
                    if (ii$other[i] != ""){
                        sep <- paste0("[",ii$other[i],"]")
                        selects <- unlist(strsplit(selects, sep))
                    }
                    #print(paste0("selects=",selects,"|"))
                    updateSelectInput(session, ii$inputId[i], selected = selects)
                }
                else if (ii$type[i] == "textInput"){
                    updateTextInput(session, ii$inputId[i], value = query[[ii$inputId[i]]])
                }
                else if (ii$type[i] == "radioButtons"){
                    updateRadioButtons(session, ii$inputId[i], selected = query[[ii$inputId[i]]])
                }
                else if (ii$type[i] == "tabsetPanel"){
                    updateTabsetPanel(session, ii$inputId[i], selected = query[[ii$inputId[i]]])
                }
            }
            #print(paste0(ii$type[i],"|",ii$inputId[i],"|",ii$default[i],"|",ii$other[i])) #DEBUG
        }
        updateCheckboxInput(session, "ignore", value = TRUE) # turn off after each execution
    }
}
