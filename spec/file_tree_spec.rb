$:.unshift "spec/"
require 'xiki/file_tree'
require 'xiki/core_ext'


describe "handles?" do
  it "handles when slash" do
    FileTree.handles?(["/tmp/", "aa/"]).should == 0
  end

  it "doesn't handle when slash not at root" do
    FileTree.handles?(["foo/", "/tmp/"]).should == nil
  end
end


describe "matches_root_pattern?" do
  it "handles root" do
    (!!FileTree.matches_root_pattern?("/")).should == true
    (!!FileTree.matches_root_pattern?("/hey")).should == true
  end

  it "handles others" do
    (!!FileTree.matches_root_pattern?("~/aa")).should == true
    (!!FileTree.matches_root_pattern?("./aa")).should == true
  end

  it "handles bookmarks" do
    (!!FileTree.matches_root_pattern?("$aa")).should == true
    (!!FileTree.matches_root_pattern?("$..")).should == false
  end

  it "declines non-file paths" do
    (!!FileTree.matches_root_pattern?("hey")).should == false
    (!!FileTree.matches_root_pattern?("hey/you")).should == false
  end
end


describe "snippet" do
  it "uses options" do
    FileTree.snippet(:txt=>"hey", :file=>"/tmp/file_tree.rb").should == "/tmp/\n  - file_tree.rb\n    | hey\n"
  end
end


# TODO 2011-11-11: turn these into specs!



#   def test_move_dir_to_junior_internal
#     before =
#       "|- /projects/foo/
#        |  - pages.rb
#        |".gsub(/^ *\|/, '')
#     after =
#       "|- /projects/
#        |  - foo/
#        |    - pages.rb
#        |".gsub(/^ *\|/, '')
#     assert_equal after, FileTree.move_dir_to_junior_internal(before)
#   end
#   def test_move_dir_to_junior_internal_indented
#     before =
#       "|  - app/controllers/
#        |    - pages.rb
#        |      |def
#        |".gsub(/^ *\|/, '')
#     after =
#       "|  - app/
#        |    - controllers/
#        |      - pages.rb
#        |        |def
#        |".gsub(/^ *\|/, '')
#     assert_equal after, FileTree.move_dir_to_junior_internal(before)
#   end
#   def test_move_dir_to_junior_internal_indented
#     before =
#       "|- app/controllers/
#        |  - pages.rb
#        |- stay
#        |".gsub(/^ *\|/, '')
#     after =
#       "|- app/
#        |  - controllers/
#        |    - pages.rb
#        |- stay
#        |".gsub(/^ *\|/, '')
#     assert_equal after, FileTree.move_dir_to_junior_internal(before)
#   end

#   def test_move_dir_to_junior_internal_indented_complex
#     before =
#       "|  - app/controllers/
#        |    - pages.rb
#        |  - stay
#        |    - stay2
#        |".gsub(/^ *\|/, '')
#     after =
#       "|  - app/
#        |    - controllers/
#        |      - pages.rb
#        |  - stay
#        |    - stay2
#        |".gsub(/^ *\|/, '')
#     assert_equal after, FileTree.move_dir_to_junior_internal(before)
#   end



describe "#extract_filters" do
  it "one file filter" do
    file_path = "/projects/foo/**ls/"
    FileTree.extract_filters!(file_path).should == ["**ls/"]
    file_path.should == "/projects/foo/"
  end

  it "one content filter" do
    file_path = "/projects/foo/##hey/"
    FileTree.extract_filters!(file_path).should == ["##hey/"]
    file_path.should == "/projects/foo/"
  end

  it "grabs two at end" do
    file_path = "/projects/foo/**index/##hey/"
    FileTree.extract_filters!(file_path).should == ["**index/", "##hey/"]
    file_path.should == "/projects/foo/"
  end

  it "removes two" do
    file_path = "/projects/foo/**index/##hey/foo"
    FileTree.extract_filters!(file_path).should == nil
    file_path.should == "/projects/foo/foo"
  end

  it "grabs one at end and removes both" do
    file_path = "/projects/##hey/foo/##hey/"
    FileTree.extract_filters!(file_path).should == ["##hey/"]
    file_path.should == "/projects/foo/"
  end

  it "removes when quote exists" do
    file_path = "/projects/foo/##hey/di/index.txt/| Hey from index"
    FileTree.extract_filters!(file_path).should == nil
    file_path.should == "/projects/foo/di/index.txt/| Hey from index"
  end

  it "removes one but leaves one in quote" do
    file_path = "/projects/foo/##hey/di/index.txt/| Hey from index/##hi/"
    FileTree.extract_filters!(file_path).should == nil
    file_path.should == "/projects/foo/di/index.txt/| Hey from index/##hi/"
  end

end

