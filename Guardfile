# Installation:
#  - check for Ruby: ruby --version
#  - install Bundler: gem install bundler
#  - install Gems: bundle install
#  - start 

require 'thread'
$debounce_timer = nil
$debounce_mutex = Mutex.new

def debounce(delay = 1.0)
  $debounce_mutex.synchronize do
    if $debounce_timer
      $debounce_timer.kill
    end

    $debounce_timer = Thread.new do
      sleep delay
      yield
    end
  end
end

guard :shell do
  # Watch any .R file except app.R
  watch(%r{^(?!app\.R$).+\.R$}) do |m|
    puts "Change detected in #{m[0]}"
    debounce(1.0) do
        puts "\n*** Touching `app.R` to trigger shiny auto reload ***"
        `touch app.R`
    end
  end
end
