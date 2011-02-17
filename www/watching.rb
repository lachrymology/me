require 'watchr'

watch('src/(.*)') {|t| system "webgen"}

