cd `dirname \`gem contents trogdoro-el4r | grep setup.rb\``
ruby setup.rb
cd bin/
ruby -S el4r-rctool -p
ruby -S el4r-rctool -i
