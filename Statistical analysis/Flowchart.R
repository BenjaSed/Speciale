source("main tests.R")

flowchart <- c()

flowchart$input <- glue(
"Patients registered in macimorelin-list
at department of endocrinology, AUH
from 17/10-2022 to 01/04-2024 
(n = 20)
Complete data available = 17
Limited data available = 3")

flowchart$maciexcluded <- glue(
  "No completed macimorelin test
(n = 2)
Stopped due to adverse reaction = 1
Patient did not show = 1"
)

flowchart$maciincluded <- glue(
  "Underwent macimorelin test
(n = 18)
Once during study period = 16
Twice during study period = 2"
)

flowchart$synacincluded <- glue(
  "Synacthen test performed
(n = 12)
Number of events = 13")

flowchart$primout <- glue(
"Included in main study
(n = 18)
Number of events = 20")

flowchart$nodual <- glue(
"Not measured with both
LC-MS/MS and immunoassay
(n = 6)
Number of events = 6
")

flowchart$secout <- glue(
"Included in sub study
(n = 8)
Number of events = 9")

flowchart$spotcorti <- glue(
  "No synacthen test
within 01/01-2020 - 01/04-2024
(n = 6)
Events with spot-cortisol
alternative = 7")



plot <- grViz("
digraph cohort_flow_chart
{
graph [splines=ortho]
node [fontname = Helvetica, fontsize = 13.7, shape = box, width = 4,]

#Descriptor nodes
identification[label = 'Identification', fillcolor = LightBlue, style = filled, width = 3, height = 0.6]
synacscreening[label = 'Synacten test screening',fillcolor = LightBlue, style = filled, width = 3, height = 0.6]
maciscreening[label = 'Macimorelin test screening',fillcolor = LightBlue, style = filled, width = 3, height = 0.6]
dualtestscreening[label = 'Dual measurement screening',fillcolor = LightBlue, style = filled, width = 3, height = 0.6]
primout[label = 'Studies', fillcolor = LightBlue, style = filled, width = 3, height = 0.6]

#Nodes
input[label = '@@1', width = 3]
maciexcluded[label = '@@2', width = 3]
maciincluded[label = '@@3', width = 3]
nodual[label = '@@4', width = 3]
synacincluded[label = '@@6', width = 3]
mainstudy[label = '@@7', width = 3, fillcolor = LightGrey, style = filled]
secstudy[label = '@@8', width = 3, fillcolor = LightGrey, style = filled]
spotcorti[label = '@@9', width = 3]
blank_1[label = '', width = 0.01, height = 0.01]
blank_2[label = '', width = 0.01, height = 0.01]
blank_4[label = '', width = 0.01, height = 0.01]

#Ranks
  { rank = same; identification input}
  { rank = same; maciscreening blank_1 maciexcluded}
  { rank = same; synacscreening spotcorti blank_2}
  { rank = same; synacincluded}
  { rank = same; dualtestscreening blank_4 nodual}
  { rank = same; primout mainstudy secstudy}
  
#Descriptor directions
	identification -> maciscreening -> synacscreening -> dualtestscreening -> primout [dir = none, color = white];

#Directions
	input -> blank_1[dir = none];
  blank_1 -> maciexcluded;
  blank_1 -> maciincluded;
  maciincluded -> blank_2[dir = none];
  blank_2 -> synacincluded;
  spotcorti -> blank_2[dir = back];
  spotcorti -> mainstudy;
  synacincluded -> mainstudy;
  synacincluded -> blank_4[dir = none];
  blank_4 -> secstudy;
  blank_4 -> nodual
}
#Nodes to data link
[1]: flowchart$input
[2]: flowchart$maciexcluded
[3]: flowchart$maciincluded
[4]: flowchart$nodual
[5]: flowchart$synacexcluded
[6]: flowchart$synacincluded
[7]: flowchart$primout
[8]: flowchart$secout
[9]: flowchart$spotcorti
")
if(showplots){plot}

if(saveplots){
  plot %>%
    export_svg() %>%
    read_xml() %>%
    write_xml(paste0(save_file_location,"/Flowchart.svg"))
  }
