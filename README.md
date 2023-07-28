# span

Span is a static-site generator based on Pandoc. It generates a static site from a folder of markdown sources. You can also easily generate a RSS feed or other non-HTML generated files using Span.


## Installation

Currently, the only way to install this is using `cabal`:

```
git clone https://github.com/PerpetualCreativity/span.git
cabal install --overwrite-policy=always
```

And make sure to have `~/.cabal/bin` in your path.

## Behavior

### Directory layout

```
./
    content/
    templates/
    output/
    span.yml
```

The **content** of the website goes in `content/`; your paths are a direct reflection of the folder structure (e.g. `/content/blog/interesting.md ` becomes `output/content/blog/interesting`). Span supports using [DocTemplate syntax](https://hackage.haskell.org/package/doctemplates-0.11/docs/Text-DocTemplates.html) within a content file; however, due to [limitations in Pandoc](https://github.com/jgm/pandoc/issues/1950), Span employs a workaround: inside content files, use `%` for DocTemplate syntax instead of `$`.

**Templates** are Pandoc templates used to render your content.

Span puts the resulting files in `output/`.

### Template selection logic

When you specify a template, Span uses the following logic for figuring out which template to use.

Consider the following site structure:

```
./
    content/
        index.md
        about.md
        blog/
            interesting.md
            awesome.md
            cool.md
        notes/
            contents.md
            2021/
                note.md
            2022/
                contents.md
                note1.md
                note2.md
    snippets/
        navbar.html
        blog/
            navbar.html
        notes/
            author.html
    templates/
        default.html
        blog/
            default.html
        notes/
            default.html
            contents.html
```

Span will use the closest matching template. Span considers files in the closest matching directory, looking in the parent only if necessary, and using a matching name (without extension), or `default.html` templates when possible. Consider `notes/2022/note1.md`. Span looks under templates, but can't find `templates/notes/2022/`, so it jumps to the parent, `templates/notes`, and looks there. This has a `default.html`, so Span will use this template to render `notes/2022/note1.md`. Below is a table of sample files and what template it will use:

| content file             | template file         |
|--------------------------|-----------------------|
| `index.md`               | `default.html`        |
| `blog/cool.md`           | `blog/default.html`   |
| `notes/contents.md`      | `notes/contents.html` |
| `notes/2022/contents.md` | `notes/contents.html` |

Thus, you must have at least a `default.html` in `templates/` for Span to work. Use `pandoc -D html > templates/default.html` to use the default pandoc template (though I recommend using a custom template).

If you want to use a partial in your templates, put them in the same directory as the template:

> partials will be sought in the directory containing the main template, and will be assumed to have the extension of the main template
- [Pandoc DocTemplates documentation](https://hackage.haskell.org/package/doctemplates-0.11/docs/Text-DocTemplates.html)

## Configuration

The config file is by default `span.yml` (you can specify another name through the command-line options):

```yml
# Ignore files in contents/ that match these glob expressions.
ignore:
- drafts/**

# Pass-through (just copy, don't run pandoc) on files in contents/ that
# match these glob expressions.
passthrough:
- games/**
- experiments/**

# Passed to every template.
globals:
  name: Span
  cool: Yeah!

# Passed to the corresponding template when this file is run.
# Equivalent to Pandoc metadata, except these can be passed to multiple files
# The actual metadata in the files (if any) override these values.
variables:
  posts:
    files:
    - blog.html
    - rss.xml
    data:
    - title: Hello
      description: Cool stuff here
      path: posts/asdf.html
    - title: Very interesting?
      description: More cool stuff here
      path: posts/asdf2.html
  news:
    files:
    - news.html
    - news.xml
    data:
    - message: Awesome news!
      date: 1970-01-01
    - message: Very cool news!
      date: 1970-01-02

# Span, by default, treats files with the `.md` extension as markdown, and
# files with the `.html` extension as html. If you want to add recognition
# for more extensions, fill this out.
# These strings are the recognized input names:
# https://hackage.haskell.org/package/pandoc-3.1.6/docs/src/Text.Pandoc.Readers.html#readers
extensions:
  md: markdown
  html: html
  # you can leave the above two out if you're just using the default values.
  xml: html # useful for RSS feeds
  typ: typst

# Run filters against files in contents/ that match these globs.
# Relative paths to filters are relative to the directory span is run in.
filters:
  "**/":
  - Lua: filters/commonfilter
  - Lua: filters/anotherfilter
  notes/*/math/**/:
  - JSON: ~/path/to/pandoc-asciimath2tex
```

This is the [glob syntax](https://hackage.haskell.org/package/Glob-0.10.2/docs/System-FilePath-Glob.html#v:compile).

### Examples?

TODO. I'd like to put an RSS feed template here, or even a "full" example.
