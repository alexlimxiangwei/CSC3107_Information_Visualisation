// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): block.with(
    fill: luma(230), 
    width: 100%, 
    inset: 8pt, 
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    new_title_block +
    old_callout.body.children.at(1))
}

#show ref: it => locate(loc => {
  let target = query(it.target, loc).first()
  if it.at("supplement", default: none) == none {
    it
    return
  }

  let sup = it.supplement.text.matches(regex("^45127368-afa1-446a-820f-fc64c546b2c5%(.*)")).at(0, default: none)
  if sup != none {
    let parent_id = sup.captures.first()
    let parent_figure = query(label(parent_id), loc).first()
    let parent_location = parent_figure.location()

    let counters = numbering(
      parent_figure.at("numbering"), 
      ..parent_figure.at("counter").at(parent_location))
      
    let subcounter = numbering(
      target.at("numbering"),
      ..target.at("counter").at(target.location()))
    
    // NOTE there's a nonbreaking space in the block below
    link(target.location(), [#parent_figure.at("supplement") #counters#subcounter])
  } else {
    it
  }
})

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      block(
        inset: 1pt, 
        width: 100%, 
        block(fill: white, width: 100%, inset: 8pt, body)))
}


#let poster(
  // The poster's size.
  size: "'36x24' or '48x36''",

  // The poster's title.
  title: "Paper Title",

  // A string of author names.
  authors: "Author Names (separated by commas)",

  // Department name.
  departments: "Department Name",

  // University logo.
  univ_logo: "Logo Path",

  // Footer text.
  // For instance, Name of Conference, Date, Location.
  // or Course Name, Date, Instructor.
  footer_text: "Footer Text",

  // Any URL, like a link to the conference website.
  footer_url: "Footer URL",

  // Email IDs of the authors.
  footer_email_ids: "Email IDs (separated by commas)",

  // Color of the footer.
  footer_color: "Hex Color Code",

  // DEFAULTS
  // ========
  // For 3-column posters, these are generally good defaults.
  // Tested on 36in x 24in, 48in x 36in, and 36in x 48in posters.
  // For 2-column posters, you may need to tweak these values.
  // See ./examples/example_2_column_18_24.typ for an example.

  // Any keywords or index terms that you want to highlight at the beginning.
  keywords: (),

  // Number of columns in the poster.
  num_columns: "3",

  // University logo's scale (in %).
  univ_logo_scale: "100",

  // University logo's column size (in in).
  univ_logo_column_size: "10",

  // Title and authors' column size (in in).
  title_column_size: "20",

  // Poster title's font size (in pt).
  title_font_size: "48",

  // Authors' font size (in pt).
  authors_font_size: "36",

  // Footer's URL and email font size (in pt).
  footer_url_font_size: "30",

  // Footer's text font size (in pt).
  footer_text_font_size: "40",

  // The poster's content.
  body
) = {
  // Set the body font.
  set text(font: "STIX Two Text", size: 16pt)
  let sizes = size.split("x")
  let width = int(sizes.at(0)) * 1in
  let height = int(sizes.at(1)) * 1in
  univ_logo_scale = int(univ_logo_scale) * 1%
  title_font_size = int(title_font_size) * 1pt
  authors_font_size = int(authors_font_size) * 1pt
  num_columns = int(num_columns)
  univ_logo_column_size = int(univ_logo_column_size) * 1in
  title_column_size = int(title_column_size) * 1in
  footer_url_font_size = int(footer_url_font_size) * 1pt
  footer_text_font_size = int(footer_text_font_size) * 1pt

  // Configure the page.
  // This poster defaults to 36in x 24in.
  set page(
    width: width,
    height: height,
    margin: 
      (top: 1in, left: 2in, right: 2in, bottom: 2in),
    footer: [
      #set align(center)
      #set text(32pt)
      #block(
        fill: rgb(footer_color),
        width: 100%,
        inset: 20pt,
        radius: 10pt,
        [
          #text(font: "Courier", size: footer_url_font_size, footer_url) 
          #h(1fr) 
          #text(size: footer_text_font_size, smallcaps(footer_text)) 
          #h(1fr) 
          #text(font: "Courier", size: footer_url_font_size, footer_email_ids)
        ]
      )
    ]
  )

  // Configure equation numbering and spacing.
  set math.equation(numbering: "(1)")
  show math.equation: set block(spacing: 0.65em)

  // Configure lists.
  set enum(indent: 10pt, body-indent: 9pt)
  set list(indent: 10pt, body-indent: 9pt)

  // Configure headings.
  set heading(numbering: "I.A.1.")
  show heading: it => locate(loc => {
    // Find out the final number of the heading counter.
    let levels = counter(heading).at(loc)
    let deepest = if levels != () {
      levels.last()
    } else {
      1
    }

    set text(24pt, weight: 400)
    if it.level == 1 [
      // First-level headings are centered smallcaps.
      #set align(center)
      #set text({ 32pt })
      #show: smallcaps
      #v(50pt, weak: true)
      #if it.numbering != none {
        numbering("I.", deepest)
        h(7pt, weak: true)
      }
      #it.body
      #v(35.75pt, weak: true)
      #line(length: 100%)
    ] else if it.level == 2 [
      // Second-level headings are run-ins.
      #set text(style: "italic")
      #v(32pt, weak: true)
      #if it.numbering != none {
        numbering("i.", deepest)
        h(7pt, weak: true)
      }
      #it.body
      #v(10pt, weak: true)
    ] else [
      // Third level headings are run-ins too, but different.
      #if it.level == 3 {
        numbering("1)", deepest)
        [ ]
      }
      _#(it.body):_
    ]
  })

  // Arranging the logo, title, authors, and department in the header.
  align(center,
    grid(
      rows: 2,
      columns: (univ_logo_column_size, title_column_size),
      column-gutter: 0pt,
      row-gutter: 50pt,
      image(univ_logo, width: univ_logo_scale),
      text(title_font_size, title + "\n\n") + 
      text(authors_font_size, emph(authors) + 
          "   (" + departments + ") "),
    )
  )

  // Start three column mode and configure paragraph properties.
  show: columns.with(num_columns, gutter: 64pt)
  set par(justify: true, first-line-indent: 0em)
  show par: set block(spacing: 0.65em)

  // Display the keywords.
  if keywords != () [
      #set text(24pt, weight: 400)
      #show "Keywords": smallcaps
      *Keywords* --- #keywords.join(", ")
  ]

  // Display the poster's contents.
  body
}
// Typst custom formats typically consist of a 'typst-template.typ' (which is
// the source code for a typst template) and a 'typst-show.typ' which calls the
// template's function (forwarding Pandoc metadata values as required)
//
// This is an example 'typst-show.typ' file (based on the default template  
// that ships with Quarto). It calls the typst function named 'article' which 
// is defined in the 'typst-template.typ' file. 
//
// If you are creating or packaging a custom typst template you will likely
// want to replace this file and 'typst-template.typ' entirely. You can find
// documentation on creating typst templates here and some examples here:
//   - https://typst.app/docs/tutorial/making-a-template/
//   - https://github.com/typst/templates

#show: doc => poster(
   title: [Inflation in the USA], 
  // TODO: use Quarto's normalized metadata.
   authors: [Michael T. Gastner], 
   departments: [Information and Communication Technologies], 
   size: "33x23", 

  // Institution logo.
   univ_logo: "./images/sit-logo.png", 

  // Footer text.
  // For instance, Name of Conference, Date, Location.
  // or Course Name, Date, Instructor.
   footer_text: [Information Visualization 2024], 

  // Any URL, like a link to the conference website.
  

  // Emails of the authors.
   footer_email_ids: [michael.gastner\@singaporetech.edu.sg], 

  // Color of the footer.
   footer_color: "ebcfb2", 

  // DEFAULTS
  // ========
  // For 3-column posters, these are generally good defaults.
  // Tested on 36in x 24in, 48in x 36in, and 36in x 48in posters.
  // For 2-column posters, you may need to tweak these values.
  // See ./examples/example_2_column_18_24.typ for an example.

  // Any keywords or index terms that you want to highlight at the beginning.
  

  // Number of columns in the poster.
  

  // University logo's scale (in %).
  

  // University logo's column size (in in).
  

  // Title and authors' column size (in in).
  

  // Poster title's font size (in pt).
  

  // Authors' font size (in pt).
  

  // Footer's URL and email font size (in pt).
  

  // Footer's text font size (in pt).
  

  doc,
)


= Introduction
<introduction>
Inflation, the rate at which the general level of prices for goods and services rises, eroding purchasing power, is a key economic indicator. It impacts everything from the cost of groceries to the interest rates on loans#footnote[https:\/\/www.hbs.edu/ris/Publication%20Files/Paper\_Covid\_Price\_IMFER\_23\_4663bd2c-c1a8-4448-aa9f-98a3bc197142.pdf];. While moderate inflation is a sign of a growing economy, excessive inflation can reduce the value of money, leading to financial strain for consumers. In this project, we have built on a comprehensive visualization to monitor inflation across various consumer metrics in the USA, originally published by NBC News #footnote[#link("https://www.nbcnews.com/data-graphics/inflation-tracker-how-much-prices-rising-us-consumers-n1296378");] \(@fig-nbc-on-poster). The visualization below shows monthly changes in consumer prices compared to the same time the previous year. Despite the visualization’s effectiveness in showing an overview of the price changes, we aim to improve certain aspects to better highlight the underlying trend of inflation change in a year-over-year basis of different consumer metrics.

= Previous Visualization
<previous-visualization>
#block[
#block[
#figure([
#box(width: 90%,image("images/inflation-original.png"))
], caption: figure.caption(
position: bottom, 
[
Yearly Percentage Change in Consumer Prices for major catergories of goods in the USA, published by NBC News.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-nbc-on-poster>


]
]
= Strengths
<strengths>
- The plot title and subtitle is clear and informative, immediately providing context and helping readers to understand the focus of the visualization.
- The heatmap design effectively conveys a high information content without cluttering the plot. This allows for quick and intuitive understanding of the data trends.
- Pointing with the mouse at a tile opens an infotip, enabling readers to retrieve specific incidence data for a given state and year \(#strong[?\@fig-infotip\_color\_change];). The infotip only occludes a small portion of the plot, and the partial transparency of the infotip ensures visibility of the tiles underneath.
- The colors chosen \(red and blue) are color-blind friendly, ensuring that the visualization is accessible to a wider audience.
- The inclusion of detailed source information at the bottom of the chart adds credibility and transparency to the data presented.
- The overall design is user-friendly, with a clean layout, intuitive color coding, and easy-to-read labels, making the visualization accessible to a broad audience.
- The color legend is clear and informative which helps readers to quickly understand the meaning of the the colors \(red-increasing inflation, blue-decreasing inflation) and the range of percentage changes displayed.

= Suggested Improvements
<suggested-improvements>
+ #emph[Adding more layers of information to the chart.]

  Information like:

  - Inflation was due to major events like COVID-19, recessions, or weather changes \(e.g.~hotter/colder seasons)
  - Inflation in different geographical locations

+ #emph[Varying the size of the square boxes] to convey additional information \(e.g.~size of industry, as each category has varying impacts on the economy.

+ #emph[Incorporate mirrored histograms, stacked histograms, or separate histograms] to present the data more effectively.

+ #emph[Shade areas] to represent significant events such as recessions or the COVID-19 pandemic.

+ #emph[Add an interactive straight line] for readers to follow across the graph, enhancing readability and comprehension.

+ #emph[Add a category like "Others"] to capture data that does not fit into the predefined categories, ensuring completeness.

= Implementation
<implementation>
== Data
<data>
- #strong[Data Source:] The data used for this project is based on the visualization data from Joella Carman and Nigel Chiwaya, featured on NBC News. They utilized the information from the Bureau of Labor Statistics and Energy Information Administration data to depict year-over-year inflation for major categories of goods.
- #strong[Data Period:] Our analysis makes use of data from January 2019 to March 2024, rather than the initial period from May 2023 to April 2024. With this longer time frame, the inflation rates for the chosen main product groups are more clearly displayed.

== Software
<software>
We used the Quarto publication framework and the R programming language, along with the following third-party packages:

- #emph[readxl] for data import
- #emph[tidyverse] for data transformation, including #emph[ggplot2] for visualization based on the grammar of graphics
- #emph[knitr] for dynamic document generation
- #emph[purrr] for functional programming tools
- #emph[pheatmap] for creating heatmaps and having more control over the dimensions and appearance
- #emph[scales] for additional scaling functions
- #emph[stringr] for string manipulation

#v(2em)
= Improved Visualization
<improved-visualization>
#block[
#block[
#box(width: 576.0pt, image("inflation_poster_files/figure-typst/plot-with-white-lines-2-1.svg"))

]
]
= Further Suggestions for Interactivity
<further-suggestions-for-interactivity>
Because our visualization was intended for a poster, we did not implement any interactive features, including the infotip. However, if the data are visualized in an HTML document, interactive features can be achieved using the R packages such as #emph[plotly];. In that case, we recommend that the tile does not change its fill color. In contrast, the original visualization changes the fill color of the activated tile to light blue \(see #strong[?\@fig-infotip\_color\_change];), which can be misinterpreted as a change in incidence. Instead, we suggest highlighting the activated tile by thickening its border.

= Conclusion
<conclusion>
We successfully implemented the suggested improvements for the non-interactive visualization, including adding layers of information about major events like COVID-19 and its impact on inflation rates across various categories, providing a richer contextual understanding of the data. Our continuous timeline and intuitive heat map color gradient from blue to red effectively illustrate trends and changes over time. Furthermore, The inclusion of COVID-19 case data adds depth, highlighting correlations between the pandemic and inflation fluctuations. These enhancements make the improved visualization more comprehensive and insightful, effectively illustrating trends and correlations over time.
