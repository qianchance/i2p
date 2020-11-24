# Week 7 Agenda

### Assessment #2

> I had interpreted it as doing the biography on just the August listing dataset we were given and then in the questions like ‘to what extent is the data complete’ I analysed whether this dataset in isolation was a complete picture of the process it claims for us to examine and whether integration of the other datasets would be more beneficial (i.e. time-series analysis for airbnb impact etc)

- The focus here is really on approaching the ‘assigned’ data with a critical eye—what are the strengths and weaknesses, what are the ethics, etc. Link it to the readings. While it is not necessary to have run any code/done any analysis, it will undoubtedly be easier for many of you to ground your thinking in concrete examples drawn from the data. Bringing other data sets from the InsideAirbnb site into it may help but might just confuse.
- It is not necessary that you perform any new work in Python for Assessment 2. If you are having trouble making *sense* of the data and feel that it would help to think more 'concretely' then you might want to revisit the summary information and plots that we did for one or more of the columns we’ve not looked at... but that is *only* if you think it will help you to answer the questions in greater detail and with more specificity. I am not asking for you to do anything more with the code than you have already done. You will not get a better mark for having written more/new code.
- The readings have invited you to think about how data is generated, for what purpose(s), who is included/excluded, what is missing, etc., as well as issues of ethics (such as the ethical use of data), and these questions and topics are intended to support a critical engagement with the data. I would start by reading up on how the data was collected and for what purpose, and then use that as a launching point for the rest of the assessment.
- It is not necessary that you look at the other data available on the InsideAirbnb web site but, again, if it helps you think through the issues in greater detail then you are free to reference them in your answer. You will not get a better mark for discussing other data sets in detail.
- Please use the template to answer the questions, you don't need to write an intro/conclusion/etc. By the time you've finished filling in the Markdown template you'll have something fairly well-organised that can, hopefully, be read as a kind of highly-structured essay.

## Agenda

- Different plotting options: https://stackoverflow.com/a/37970713
  - Short Answer: you don't, I'm a creature of habit and lazy.
  - Longer Answer: I prefer to show you *one* way that works generally, than two or three different ways that require you to change your code for each.
- Floating Point Arithmetic
  - Hat tip for this: https://github.com/jreades/i2p/issues/15
  - Floating Point errors are a *fundamental* cause of problems in many applications and they are *hard* to debug.

## Techniques

- CSLs and LaTeX:
  - Short answer: [no](https://tex.stackexchange.com/a/69284)
  - For full LaTeX bibliographies you will need either BibLaTeX or Biber as these are more powerful and offer more options.
  - CSLs are for pandoc alone AFAIK.
  - Here's a nice RMarkdown tutorial: https://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html
- RegExes
  - Part 1: `[A-F]` has the special meaning 'A-through-F' (i.e. `A|B|C|D|E|F`); whereas the `{m,n}` syntax is for something _repeating_ between `m` and `n` times (inclusive). So `m` and `n` must be numeric. I think you're being misled by Python's dictionary sytnax?
  - Part 2: `^` has _two_ meanings:
    - Inside a `[...]` and as the first character it means _not_ (i.e. `[^A-F]`) would negate 'A-through-F'.
    - At the start of a regular expression it means _at the start of a line_ (i.e. `r'^A'` would match lines starting with an `A`).
- Fit/Transform
  - Fitting and transformation do not have to happen at the same time.
  - But fitting cannot be updated _afterwards_ as the mapping/weights have been calculated.
  - So values that weren't fitted are likely to be dropped during the transform stage because there's no mapping/weight for them.
  - You might have separate fit and transform calls if you don't want to/need to transform all of your data at once and only want to transform parts of the full data set. So you fit once and then transform multiple times.
  - You might also have separate fit and transform calls if your data is streaming (but see limitation above).
- How you do you add a bib file to Markdown?
  - Yes, only pandoc can make sense of the bib file _in Markdown_. 
  - LaTeX can process bib files too though.

## Concepts

- Can we measure the complexity of a text?
  - Up to a point: there are all sorts of ways to measure this! What is meaningful depends on the language, sentence structure, grammar, etc.! 
- Shared readings!!!
  - https://collections.plos.org/collection/science-of-stories/
  - https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0225385
  - There are a lot more on this! Great finds though!

## Triumphs

- Using `numpy`
  - Very nice! 🎩tip
  - Using numpy directly will definitely represent a speed-up, but you would only benefit for queries that numpy can cope with (primarily NaNs and Numbers--which is a pretty big span!)

- Pronunciation
  - Hah, hah, you're not the only one!