;; Project Rakefile generation template

(setq unity-rakefile "

PROJECT_ROOT  = File.expand_path( File.dirname(__FILE__) )

load File.join(File.dirname(__FILE__),'ceedling-rakefile-target.rb')

task :default => [:clobber, 'test:all']

")

(provide 'unity-rakefile)
