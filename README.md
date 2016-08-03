# viztrackr

Documentation forthcoming.

# Installation of supporting applications
General procedure that was followed to install:

* Mac only: install homebrew, xcode command line tools
* Install ruby, rails (>=4.0)
* Install blacklight and set up blacklight app:
    * `rails new recordr`
    * `cd recordr`
    * `echo "gem 'blacklight', '>= 5.3.0'" >> Gemfile`
    * `echo "gem 'blacklight-gallery'" >> Gemfile`
    * `echo "gem 'blacklight_range_limit'" >> Gemfile`
    * `echo "gem 'blacklight_advanced_search'" >> Gemfile`
    * `echo "gem 'blacklight-oembed'" >> Gemfile`
    * `echo "gem 'tzinfo-data', platforms: [:mingw, :mswin]" >> Gemfile`
        * (may need to uncomment `therubyracer` line in Gemfile)
    * change "gem 'sass-rails', '~>4.0.0'" to "gem 'sass-rails', '>= 4.0.0'" if applicable
        * this avoids a css generation bug with particular versions of blacklight
    * `bundle install`
    * `rails generate blacklight:install --devise --jettywrapper`
    * `rails generate blacklight_gallery:install`
    * `rails generate blacklight_range_limit`
    * `rails generate blacklight_advanced_search:install`
    * `rake db:migrate`
    * `rake jetty:clean`
        * (note that the last step may take quite a while to complete)
* If necessary Update the solr version distributed with blacklight
    * change the mode of the `jetty/update_solr` script to executable
    * run `jetty/update_solr` with the version of solr you'd like to install as an argument
* Configure blacklight:
    * if necessary, remove references to Openseadragon in `app/controllers/application_controller.rb` and `app/controllers/catalog_controller.rb` (added by `blacklight_gallery`)
    * edit `config/blacklight.yml`: change `blacklight-core` to name of your solr core
    * edit `config/jetty.yml`: change `jetty_port` to desired port
    * edit `config/blacklight.yml`: change port where rails app expects to find solr running
    * create a new directory in `jetty/solr/` for your solr core. to start, can copy contents of `jetty/solr/blacklight-core` to the `conf/` subdirectory of this directory, then delete `elevate.xml` (along with any other unused files) and add admin-extra*.html files from `jetty/tmp/solr-5.2.1/server/solr/configsets/sample_techproducts_configs/conf/` and the entire lang directory from `jetty/tmp/solr-5.2.1/server/solr/configsets/basic_configs/conf/` (only necessary if using `lang/stopwords_en.txt` in config).
    * edit `jetty/solr/<yourcore>/conf/solrconfig.xml` and jetty/solr/<yourcore>/conf/schema.xml` or copy in new versions. (note that deprecated fields like `solr.IntField` may need to be removed to even get solr to initialize.)
    * configure facet fields, index fields, solr search params logic, etc. in `/app/controllers/catalog_controller.rb`.
    * add helper function for image rendering to `/app/helpers/application_helper.rb`.
    * customize initial catalog view: create `/app/views/catalog` directory, if it doesn't exist, and copy `~/.rbenv/versions/2.2.2/lib/ruby/gems/2.2.0/gems/blacklight-5.13.1/app/views/catalog/_home_text.html.erb` to `/app/views/catalog/_home_text.html.erb`, then edit.
* Configure Solr to be compatible with blacklight:
    * disable "/select" requestHandler in `jetty/solr/<yourcore>/conf/solrconfig.xml`
    * implement "search" and "document" requestHandlers in `jetty/solr/<yourcore>/conf/solrconfig.xml`
* Start Solr
* Add data to Solr (can be accomplished via curl, recordr package, etc. -- for now, images for search result thumbnails should be placed in `/app/assets/images/`.)
* Start rails app (rails server from blacklight installation dir)
* If a ruby/rails command gets stuck, run `spring stop`.


## To Do

### First priority:

* knitr hooks: need to catch all plots created in a chunk, not just last one, and need to catch base graphics plots as well. this may involve a different strategy altogether.
* improved code functionality such that only the necessary code (from the entire document/history) to recreate the plot is retained.
* data.frames and/or CSVs as simpler data store option (and add a vignette demonstrating use)
* implement methods for SolrFrame, SolrCore, etc. in addition to SolrList
* Predicted/inferred "plot type" -- classification, auto-tagging with plot type, data/data structure, etc. Maybe use text mining/sentiment analysis?
* Cognostics/data summaries, including axis min/max, # of observations in each facet, median/min/max of continuous variables, etc. (look at trelliscope).
* add integration/unit/regression tests, including tests for features not yet implemented
* Add package description to documentation
* Webform or similar method by which to generate an RSS feed url from a query
* add tryCatch to fullData, particularly for lattice

### Second priority:

* axis scale/transformation (identity, sqrt, log, etc.) for all cases, including inferred from axis title
* Extract all "scales" and related elements from ggplot:  add total # of scales in use. z axis labels. subsetting. axis transformation. positioning system (continuous, discrete, date/datetime). labels for levels used in conditioning, as well as the aesthetic values that they map to: color/fill, marker type/shape, line type, faceting/paneling (done), alpha/transparency, size/area, theme.
* better annotation text (including all text not otherwise categorized)
* legend text/contents
* ggplot: suppress all messages (still coming up under certain conditions)
* base graphics: can we add tags to gTree or recordedPlot object directly?
* ggplot/lattice: more elegant way to add tags to plots directly.
* lattice: need legend, misc. text (text, mtext), annotations (parse out expressions), group variable types, group variable labels
* base graphics: capture grob names
* create update script for removing and re-adding when schema is updated, retaining date, user, etc. Can read other metadata from "enhanced" png.
* prepare for `proto` --> `ggproto` migration
* Parsed/structured tags
* correctly fill in ggplot defaults - these are sort of working for now, but don't always get overwritten correctly when other options are specified
* fill in lattice defaults
* add functionality for other (popular) grid-based plotting functions/packages -- or just a gTree/gtable fallback when nothing else works. Could also pass in a function for reading data out of the object. (ExpressionPlot, Gene Sets, data.frames, tables, etc.)
* Improve hashing --> unique ID, or at least allow user the option to duplicate vs overwrite vs keep original.
* Keep adding more complex/real-world training sets, especially for base graphics
* Approximate human-readable color names (as attribute? additional "named color" field?)

```
hex2name <- function(hex.code){
    colors.distinct <- colors(distinct = TRUE)
    r.color.list <- col2rgb(colors.distinct)
    r.color.list <- apply(r.color.list, 2, as.list)
    names(r.color.list) <- colors.distinct
    color.dists <- sapply(
        lapply(r.color.list, rbind, 
            as.vector(col2rgb(hex.code))), dist)
    my.color.name <- names(which.min(color.dists))
    if(min(color.dists)>0){
        my.color.name <- paste(my.color.name, "(approx)")
    }
    return(my.color.name)
}

hex2name("#3366FF")
```

* Named/human-readable line type, point type/shape
* Relationships between plots -- tree-like structure? Want to be able to tell/indicate/track when one plot is a revised version of another plot -- all should live in the data store and should link to one another (old/new/superseded versions)
* Validity checks for objects/classes? or specific list types (character lists only, for example)?
* store in data frame as a list of vectors using I()?
* if(interactive()) { check history() and look up call stack }

## Future Ideas

* Switch from vanilla Blacklight to [Sufia](https://github.com/projecthydra/sufia/) (which includes Blacklight) or Blacklight with [Spotlight](https://github.com/sul-dlss/spotlight/)?
* Better storage location for PNG versions of plots and thumbnails -- upload in R via RESTful protocol rather than saving to an obscure assets directory? Some options:
    * [Paperclip ruby gem](https://github.com/thoughtbot/paperclip/)
    * [Rails API + paperclip for image upload](http://paoloibarra.com/2014/09/27/Image-Upload-Using-Rails-API-And-Paperclip/)
    * [Rails + grape API for file upload](http://omarfouad.com/blog/2013/08/18/file-upload-api-with-grape-and-rails/)
    * [Rails RESTful API for file upload](http://stackoverflow.com/questions/14174044/uploading-a-file-in-rails)
    * [Rails JSON carrierwave/RESTful API controller code for file upload](https://gist.github.com/ifightcrime/9291167a0a4367bb55a2)
    * [Ruby gems for file uploads in Rails](https://www.ruby-toolbox.com/categories/rails_file_uploads)
    * [Dragonfly image/asset management in Rails](http://markevans.github.io/dragonfly/rails/)
* ImageMagick on backend to listen on a folder, automatically resize for feed/search result preview (and strip out EXIF data).
