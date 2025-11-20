library(DiagrammeR)

# Data
page_labels <- c(
  "Age Verification", "Demographic Information", "COVID-19 Related Questions",
  "Contact Networks", "Contact Networks 2", "Contact Networks 3", 
  "Situations During the Pandemic", "Vaccinations", 
  "About You", "Contact and Further Surveys"
)
# extracted from raw data
dropouts <- c(2, 11, 96, 152, 34, 6, 1, 14, 153, 398)
total_start <- 867

# Calculate remaining and percentages
remaining <- total_start - cumsum(dropouts)
remaining <- c(total_start, remaining[-length(remaining)])
dropout_pct <- round(dropouts / remaining * 100, 1)

# Create PRISMA-style flowchart with sections
sections <- grViz(paste0("
digraph prisma_flow {
  graph [rankdir=TB, splines=ortho, nodesep=0.8, ranksep=0.5]
  node [fontname='Arial', fontsize=10]
  edge [color='#374151', penwidth=1.5]
  
  // Starting point
  start [label='Participants started survey\\nN = ", total_start, "', 
         shape=box, style='filled,rounded', fillcolor='#3b82f6', fontcolor='white',
         width=3.5, height=0.8, fontsize=11]
  
  // SECTION 1: Demographic Information
  subgraph cluster_0 {
    label='Demographic Information'
    style='filled,rounded'
    color='#bfdbfe'
    fillcolor='#eff6ff'
    fontsize=12
    fontcolor='#1e40af'
    
    after_age [label='After ", page_labels[1], "\\nN = ", remaining[2], "', 
               shape=box, style='filled,rounded', fillcolor='#dbeafe', width=2.8, height=0.6]
    
    after_demo [label='After ", page_labels[2], "\\nN = ", remaining[3], "', 
                shape=box, style='filled,rounded', fillcolor='#dbeafe', width=2.8, height=0.6]
    
    excl_age [label='Dropped out\\nN = ", dropouts[1], " (", dropout_pct[1], "%)', 
              shape=box, style='filled', fillcolor='#86efac', width=2, height=0.6]
    
    excl_demo [label='Dropped out\\nN = ", dropouts[2], " (", dropout_pct[2], "%)', 
               shape=box, style='filled', fillcolor='#fde047', width=2, height=0.6]
    
    {rank=same; after_age; excl_age}
    {rank=same; after_demo; excl_demo}
    
    after_age -> after_demo
    after_age -> excl_age [style=dashed]
    after_demo -> excl_demo [style=dashed]
  }
  
  // SECTION 2: COVID-19 Questions
  subgraph cluster_1 {
    label='COVID-19 Information'
    style='filled,rounded'
    color='#fde68a'
    fillcolor='#fefce8'
    fontsize=12
    fontcolor='#a16207'
    
    after_covid [label='After ", page_labels[3], "\\nN = ", remaining[4], "', 
                 shape=box, style='filled,rounded', fillcolor='#fef3c7', width=2.8, height=0.6]
    
    excl_covid [label='Dropped out\\nN = ", dropouts[3], " (", dropout_pct[3], "%)', 
                shape=box, style='filled', fillcolor='#fb923c', width=2, height=0.6]
    
    {rank=same; after_covid; excl_covid}
    
    after_covid -> excl_covid [style=dashed]
  }
  
  // SECTION 3: Contact Networks (Critical section)
  subgraph cluster_2 {
    label='Contact Networks Section (Highest Dropout Zone)'
    labeljust='r'
    style='filled,rounded'
    color='#fca5a5'
    fillcolor='#fef2f2'
    fontsize=12
    fontcolor='#991b1b'
    
    after_contact1 [label='After ", page_labels[4], "\\nN = ", remaining[5], "', 
                    shape=box, style='filled,rounded', fillcolor='#fee2e2', width=2.8, height=0.6]
    
    after_contact2 [label='After ", page_labels[5], "\\nN = ", remaining[6], "', 
                    shape=box, style='filled,rounded', fillcolor='#fee2e2', width=2.8, height=0.6]
    
    after_contact3 [label='After ", page_labels[6], "\\nN = ", remaining[7], "', 
                    shape=box, style='filled,rounded', fillcolor='#fee2e2', width=2.8, height=0.6]
    
    excl_contact1 [label='Dropped out\\nN = ", dropouts[4], " (", dropout_pct[4], "%)', 
                   shape=box, style='filled', fillcolor='#ef4444', fontcolor='white', width=2, height=0.6]
    
    excl_contact2 [label='Dropped out\\nN = ", dropouts[5], " (", dropout_pct[5], "%)', 
                   shape=box, style='filled', fillcolor='#fb923c', width=2, height=0.6]
    
    excl_contact3 [label='Dropped out\\nN = ", dropouts[6], " (", dropout_pct[6], "%)', 
                   shape=box, style='filled', fillcolor='#86efac', width=2, height=0.6]
    
    {rank=same; after_contact1; excl_contact1}
    {rank=same; after_contact2; excl_contact2}
    {rank=same; after_contact3; excl_contact3}
    
    after_contact1 -> after_contact2
    after_contact2 -> after_contact3
    after_contact1 -> excl_contact1 [style=dashed]
    after_contact2 -> excl_contact2 [style=dashed]
    after_contact3 -> excl_contact3 [style=dashed]
  }
  
  // SECTION 4: Final Questions
  subgraph cluster_3 {
    label='Final Questions'
    style='filled,rounded'
    color='#bfdbfe'
    fillcolor='#eff6ff'
    fontsize=12
    fontcolor='#1e40af'
    
    after_pandemic [label='After ", page_labels[7], "\\nN = ", remaining[8], "', 
                    shape=box, style='filled,rounded', fillcolor='#dbeafe', width=2.8, height=0.6]
    
    after_vacc [label='After ", page_labels[8], "\\nN = ", remaining[9], "', 
                shape=box, style='filled,rounded', fillcolor='#dbeafe', width=2.8, height=0.6]
    
    after_about [label='After ", page_labels[9], "\\nN = ", remaining[10], "', 
                 shape=box, style='filled,rounded', fillcolor='#dbeafe', width=2.8, height=0.6]
    
    excl_pandemic [label='Dropped out\\nN = ", dropouts[7], " (", dropout_pct[7], "%)', 
                   shape=box, style='filled', fillcolor='#86efac', width=2, height=0.6]
    
    excl_vacc [label='Dropped out\\nN = ", dropouts[8], " (", dropout_pct[8], "%)', 
               shape=box, style='filled', fillcolor='#fde047', width=2, height=0.6]
    
    excl_about [label='Dropped out\\nN = ", dropouts[9], " (", dropout_pct[9], "%)', 
                shape=box, style='filled', fillcolor='#ef4444', fontcolor='white', width=2, height=0.6]
    
    {rank=same; after_pandemic; excl_pandemic}
    {rank=same; after_vacc; excl_vacc}
    {rank=same; after_about; excl_about}
    
    after_pandemic -> after_vacc
    after_vacc -> after_about
    after_pandemic -> excl_pandemic [style=dashed]
    after_vacc -> excl_vacc [style=dashed]
    after_about -> excl_about [style=dashed]
  }
  
  // Final completion
  completed [label='Survey completed\\nN = ", remaining[10], " (", 
             round(remaining[10]/total_start*100, 1), "%)', 
             shape=box, style='filled,rounded', fillcolor='#22c55e', fontcolor='white',
             width=3.5, height=0.8, fontsize=11]
  
  // Connect sections
  start -> after_age
  after_demo -> after_covid
  after_covid -> after_contact1
  after_contact3 -> after_pandemic
  after_about -> completed
  
  // Legend
  labelloc='b'
  label=<
    <table border='0' cellspacing='8'>
      <tr>
        <td colspan='8' align='center'><b>Dropout Severity</b></td>
      </tr>
      <tr>
        <td bgcolor='#ef4444' width='18' height='18'></td>
        <td align='left'>High (≥15%)</td>
        <td width='20'></td>
        <td bgcolor='#fb923c' width='18' height='18'></td>
        <td align='left'>Medium (5-15%)</td>
        <td width='20'></td>
        <td bgcolor='#fde047' width='18' height='18'></td>
        <td align='left'>Low-Med (2-5%)</td>
        <td width='20'></td>
        <td bgcolor='#86efac' width='18' height='18'></td>
        <td align='left'>Low (&lt;2%)</td>
      </tr>
    </table>
  >
}
"))

# Display the diagram
print(sections)

# Save the diagram
# Export as PNG
library(DiagrammeRsvg)
library(rsvg)

sections %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_png(here("plots", "participant_dropout_sections.png"), width = 1200)

# Create PRISMA-style flowchart without sections

nosections <- grViz(paste0("
digraph prisma_flow {
  graph [rankdir=TB, splines=ortho, nodesep=0.8, ranksep=0.5]
  node [fontname='Arial', fontsize=10]
  edge [color='#374151', penwidth=1.5]
  
  // Starting point
  start [label='Participants started survey\\nN = ", total_start, "', 
         shape=box, style='filled,rounded', fillcolor='#3b82f6', fontcolor='white',
         width=3.5, height=0.8, fontsize=11]
  
  // SECTION 1: Age + Demographic Information (combined)
  age_demo [label='After Age Verification & Demographic Information\\nN = ", remaining[3], "', 
             shape=box, style='filled,rounded', fillcolor='#dbeafe', width=3.5, height=0.8]
  
  excl_age_demo [label='Dropped out\\nN = ", dropouts[1]+dropouts[2], " (", 
             round((dropouts[1]+dropouts[2])/total_start*100,1), "%)', 
                 shape=box, style='filled', fillcolor='#fde047', width=2.5, height=0.6]
  
  {rank=same; age_demo; excl_age_demo}
  start -> age_demo
  age_demo -> excl_age_demo [style=dashed]
  
  // SECTION 2: COVID-19 Questions
  covid [label='After COVID-19 Related Questions\\nN = ", remaining[4], "', 
         shape=box, style='filled,rounded', fillcolor='#fef3c7', width=3.5, height=0.8]
  excl_covid [label='Dropped out\\nN = ", dropouts[3], " (", dropout_pct[3], "%)', 
              shape=box, style='filled', fillcolor='#fb923c', width=2.5, height=0.6]
  
  {rank=same; covid; excl_covid}
  age_demo -> covid
  covid -> excl_covid [style=dashed]
  
  // SECTION 3: Contact Networks (combined)
  contact [label='After Contact Networks Section\\nN = ", remaining[7], "', 
           shape=box, style='filled,rounded', fillcolor='#fee2e2', width=3.5, height=0.8]
  excl_contact [label='Dropped out\\nN = ", dropouts[4]+dropouts[5]+dropouts[6], " (", 
             round((dropouts[4]+dropouts[5]+dropouts[6])/total_start*100,1), "%)', 
                 shape=box, style='filled', fillcolor='#ef4444', fontcolor='white', width=2.5, height=0.6]
  
  {rank=same; contact; excl_contact}
  covid -> contact
  contact -> excl_contact [style=dashed]
  
  // SECTION 4: Behavior, Vaccinations & Additional Demographic Information (combined)
  final [label='After Behavior, Vaccinations &\\n Additional Demographic Information\\nN = ", remaining[10], "', 
         shape=box, style='filled,rounded', fillcolor='#dbeafe', width=3.5, height=0.8]
  excl_final [label='Dropped out\\nN = ", dropouts[7]+dropouts[8]+dropouts[9], " (", 
             round((dropouts[7]+dropouts[8]+dropouts[9])/total_start*100,1), "%)', 
              shape=box, style='filled', fillcolor='#fde047', width=2.5, height=0.6]
  
  {rank=same; final; excl_final}
  contact -> final
  final -> excl_final [style=dashed]
  
  // Completion
  completed [label='Survey completed\\nN = ", remaining[10], " (", 
             round(remaining[10]/total_start*100, 1), "%)', 
             shape=box, style='filled,rounded', fillcolor='#22c55e', fontcolor='white',
             width=3.5, height=0.8]
  final -> completed
  
  // Legend
  labelloc='b'
  label=<
    <table border='0' cellspacing='8'>
      <tr>
        <td colspan='8' align='center'><b>Dropout Severity</b></td>
      </tr>
      <tr>
        <td bgcolor='#ef4444' width='18' height='18'></td>
        <td align='left'>High (≥15%)</td>
        <td width='20'></td>
        <td bgcolor='#fb923c' width='18' height='18'></td>
        <td align='left'>Medium (5-15%)</td>
        <td width='20'></td>
        <td bgcolor='#fde047' width='18' height='18'></td>
        <td align='left'>Low-Med (2-5%)</td>
        <td width='20'></td>
      </tr>
    </table>
  >
}
"))

# Display the diagram
print(nosections)

# Save the diagram
# Export as PNG

nosections %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_png(here("plots", "participant_dropout_nosections.png"), width = 1200)
