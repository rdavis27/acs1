shinyUI(pageWithSidebar(
    headerPanel("American Community Survey (ACS) Data"),
    tabsetPanel(id = "tabs",
        tabPanel("Output",
            sidebarPanel(
                width = 2,
                splitLayout(
                 numericInput("minyear", "First Year", min = 1980, max = 2021, value = 2021),
                 numericInput("maxyear", "Last Year", min = 1980, max = 2021, value = 2021)
                ),
                # List fields that can be searched
                splitLayout(
                 textInput("STATE", "STATE", value = "CA"),
                 numericInput("stepyear", "Step Year", min = 1, value = 1)
                ),
                splitLayout(
                 textInput("COUNTY", "COUNTY", value = ""),
                 textInput("METRO", "METRO", value = "")
                ),
                # List fields to be sorted by
                selectInput("units", "Units",
                         choices = c("Count","Percent","Percent in group",
                                     "Mean Wage in 1000s of $","Mean Wage in 1000s of 2018$"),
                         selected = c("Percent"), multiple = FALSE),
                selectInput("geo", "Geography",
                         choices = c("STATE","COUNTY","METRO","NATION"),
                         selected = c("COUNTY"), multiple = FALSE),
                checkboxInput("reload", "Reload", value = TRUE),
                checkboxInput("occ1990", "Use 1990 Occupations", value = FALSE),
                selectInput("occ", "Occupation",
                         choices = c("All (0-9999)",
                                     "N/A (< 16 years old/never worked/NILF > 5 years)",
                                     "Management, Business, Science, & Arts Occupations",
                                     "Business Operations Specialists",
                                     "Financial Specialists",
                                     "Software developers",
                                     "Programmers, Software developers, QA & testers",
                                     "Computer and Mathematical Occupations",
                                     "Architecture and Engineering Occupations",
                                     "Life, Physical, and Social Science Occupations",
                                     "Community and Social Services Occupations",
                                     "Legal Occupations",
                                     "Education, Training, and Library Occupations",
                                     "Arts, Design, Entertainment, Sports, & Media Occs",
                                     "Healthcare Practitioners & Technical Occupations",
                                     "Healthcare Support Occupations",
                                     "Protective Service Occupations",
                                     "Food Preparation and Serving Occupations",
                                     "Building & Grounds Cleaning & Maintenance Occs",
                                     "Personal Care and Service Occupations",
                                     "Sales and Related Occupations",
                                     "Office and Administrative Support Occupations",
                                     "Farming, Fishing, and Forestry Occupations",
                                     "Construction and Extraction Occupations",
                                     "Extraction Workers",
                                     "Installation, Maintenance, and Repair Workers",
                                     "Production Occupations",
                                     "Transportation and Material Moving Occupations",
                                     "Military Specific Occupations",
                                     "Unemployed, No Work in 5 Years or More"),
                         selected = c("Programmers, Software developers, QA & testers"),
                         multiple = FALSE),
                selectInput("empstat", "Employment status",
                         choices = c("All","Employed","Unemployed","In labor force"),
                         selected = c("Employed"),
                         multiple = FALSE),
                selectInput("rowvar", "Row var",
                         choices = c("CITIZENSHIP","CITIZENSHIP2","CTZNSHP","DEGFIELD","DEGFIELD2","EDUC","EDUCATION",
                                     "MASTERS","EMPS","EMPSTAT","HISPAN","HISPRACE","RACE","RACE4","ASIAN","SEX","SEX_M_F","AGE5",
                                     "AGE10","INCWAGE20","WKSWORK2","BIRTHPLACE","BIRTHPLACE2","BIRTHPLACE_TECH",
                                     "OCC1990","OCC","OCC_CIS","OCC_COMPUTER","POVERTY200","POVERTY300","(default)"),
                         selected = c("(default)"), multiple = FALSE),
                selectInput("group", "Group by (select up to 2)",
                        choices = c("CITIZENSHIP","CITIZENSHIP2","CTZNSHP","DEGFIELD","DEGFIELD2","EDUC","EDUCATION",
                                    "MASTERS","EMPS","EMPSTAT","HISPAN","HISPRACE","RACE","RACE4","ASIAN","SEX","SEX_M_F","AGE5",
                                    "AGE10","INCWAGE20","WKSWORK2","BIRTHPLACE","BIRTHPLACE2","BIRTHPLACE_TECH",
                                    "OCC1990","OCC","OCC_CIS","OCC_COMPUTER","POVERTY200","POVERTY300"),
                            selected = c("EDUCATION"), multiple = TRUE),
                splitLayout(
                 numericInput("sortn", "Sort column", value = 4),
                 numericInput("decplaces","Decimal Places", value = "1")
                ),
                radioButtons("sortdir", NULL, c("Ascending","Descending"), "Descending", inline = TRUE),
                # List choice and selected checkboxes for selecting which fields to display
                # List options for maximum column width, total width, and total rows
                splitLayout(
                 numericInput("mincount", "Minimum Count", value = "5000"),
                 numericInput("hdrwidth", "Max Hdr Width", value = "40")
                ),
                splitLayout(
                 numericInput("geowidth", "Max Geo Width", value = "40"),
                 numericInput("colwidth", "Max Col Width", value = "40")
                ),
                numericInput("totwidth", "Maximum Total Width",  value = "240"),
                numericInput("totrows",  "Maximum Total Rows",   value = "900"),
                # Range fields
                selectInput("xrange1", "Range field1",
                         choices = c("","AGE","BPL","CITIZEN","CLASSWKR","DEGFIELD","DEGFIELD2",
                                     "EDUCD","EMPSTAT","EMPSTATD","HISPRACE","INCWAGE","IND","OCC",
                                     "POVERTY","PUMA","RACE","SEX","WKSWORK2","YRIMMIG"),
                         selected = c(""), multiple = FALSE),
                splitLayout(
                 numericInput("minrange1", "Minimum range1", min = 0, value = 0),
                 numericInput("maxrange1", "Maximum range1", min = 0, value = 0)
                ),
                selectInput("xrange2", "Range field2",
                            # choices = c("","AGE","BPL","CITIZEN","CLASSWKR","DEGFIELD","DEGFIELD2",
                            #             "EDUCD","EMPSTAT","EMPSTATD","HISPRACE","INCWAGE","IND","OCC",
                            #             "POVERY200","POVERTY300","PUMA","RACE",
                            #             "SEX","WKSWORK2","YRIMMIG"),
                            choices = c("","AGE","BPL","CITIZEN","CLASSWKR","DEGFIELD","DEGFIELD2",
                                        "EDUCD","EMPSTAT","EMPSTATD","HISPRACE","INCWAGE","IND","OCC",
                                        "POVERTY","PUMA","RACE","SEX","WKSWORK2","YRIMMIG"),
                            selected = c(""), multiple = FALSE),
                splitLayout(
                 numericInput("minrange2", "Minimum range2", min = 0, value = 0),
                 numericInput("maxrange2", "Maximum range2", min = 0, value = 0)
                ),
                textInput("showcol", "Show Column", value = ""),
                splitLayout(
                    textInput("csvname", "CSV Name", value = ""),
                    actionButton("savecsv", "Save",
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                checkboxInput("ignore", "Ignore URL Parameters")
            ),
            mainPanel(
                width = 10,
                verbatimTextOutput("myText")
            )
        ),
        tabPanel(
            "Plot",
            sidebarPanel(
                width = 2,
                splitLayout(
                    numericInput("cmin", "Col Min", value = 0),
                    numericInput("cmax", "Col Max", value = 0)
                ),
                splitLayout(
                    numericInput("xmin", "X Min", value = 0),
                    numericInput("xmax", "X Max", value = 0),
                    numericInput("xstp", "X Step", value = 0)
                ),
                splitLayout(
                    numericInput("ymin", "Y Min", value = 0),
                    numericInput("ymax", "Y Max", value = 0),
                    numericInput("ystp", "Y Step", value = 0)
                ),
                textInput("color", "Color", value = "Set3"),
                selectInput("geomtype", "Plot Type",
                            choices = c("Bar Plot","Line Graph"),
                            selected = c("Line Graph"),
                            multiple = FALSE),
                checkboxInput("xnum", "X Numeric"),
                checkboxInput("linetype", "Use linetype"),
                checkboxInput("flipxy", "Flip X&Y"),
                textInput("ptitle", "Plot Title", value = ""),
                textInput("pxremove", "Remove from xlabel", value = "")
            ),
            mainPanel(
                width = 10,
                imageOutput("myggPlot")
            )
        ),
        tabPanel(
            "Map",
            sidebarPanel(
                width = 2,
                splitLayout(
                    numericInput("minpop", "Min Pop", value = 200000),
                    numericInput("longoff", "Long Offset", value = 0.5)
                ),
                textInput("skipcity", "Cities to Skip", value = "Oakland,Long Beach"),
                textInput("showcity", "Cities to Show", value = "Santa Barbara,Santa Rosa,San Rafael,Santa Cruz"),
                textInput("mapcolors", "Map Colors", value = "lightblue1,green,yellow,orange,red"),
                checkboxInput("incstate", "Include State(s)", value = TRUE)
            ),
            mainPanel(
                width = 10,
                imageOutput("myggMap")
            )
        ),
        tabPanel("Usage",
            width = 10,
            htmlOutput(outputId = "myUsage")
        )
    ),
    mainPanel(
        
    )
))