$:.unshift "spec/"
require 'rubygems'
class Keys
  def self.method_missing(meth, *args, &block); end
end
require 'remote'
require 'core_ext'

describe Remote, "#calculate_local_path" do
  it "should put dashes in path" do
    Remote.calculate_local_path("/tmp/foo.txt", "server.com").should == "/tmp/remote_rb/server.com,tmp,foo.txt"
  end
end

describe Remote, "#calculate_remote_path" do
  it "should remove dashes from path" do
    Remote.calculate_remote_path("/tmp/remote_rb/server.com,tmp,foo.txt").should == "/tmp/foo.txt"
  end
end

# describe Remote, "#save_file" do
#   it "should save local file if modified" do
#     View = mock 'View'
#     View.should_receive(:path).and_return '/tmp/remote_rb/tmp,hey.txt'
#     Remote.save_file
#   end
# end


describe Remote, "#save_file" do
  before(:each) do
    $el = mock 'el'
    $el.stub!(:buffer_modified_p).and_return false
    view = mock 'View'
    view.stub!(:path).and_return '/tmp/remote_rb/tmp,hey.txt'
    view.stub!(:file)#.and_return '/tmp/remote_rb/tmp,hey.txt'
    view.stub!(:beep)#.and_return '/tmp/remote_rb/tmp,hey.txt'
  end

  it "should save remotely" do
    #     Remote.save_file
  end
end
