# Ruby guard file (need ruby >2.2 and bundler installed: 'gem install bundler')
# To make sure all the gems are installed, run 'bundle install' once in terminal
# Then you can use the makefile target 'make gui_dev' to start the GUI in development mode
# For browser livereload to work, need the browser extension: http://livereload.com/extensions/#installing-sections
# If the delay is too short for relaunching the app, increase the grace_period
# NOTE: not watching namespace because changes there tend to crash the process,
# manually restart guard instead when chaning namespace

guard 'process', name: 'Shiny', command: ['R', '-e', " \
rm(list = ls()); Sys.setenv('LOG_LEVEL' = 'DEBUG'); Sys.setenv('GEOPATHS_DEV'= 'ON'); shiny::runApp('app.R', port = 4446)"] do
  watch(%r{.+\.R$})
end

guard 'livereload', grace_period: 5 do
  watch(%r{.+\.R$})
end
