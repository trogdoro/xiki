$:.unshift "spec/"
require './spec/spec_helper'

require 'xiki/core/git'

describe Git, "#status_to_hash_new" do
  it "Populates untracked key" do
    txt = "
      ?? a.txt
      ?? b.txt
    ".unindent
    Git.status_to_hash(txt).should == {:untracked=>[["untracked", "a.txt"], ["untracked", "b.txt"]], :unadded=>[], :added=>[]}
  end

  it "Populates unadded key" do
    txt = "
      AM a.txt
       M committed.txt
       D deleteme.txt
    ".unindent
    Git.status_to_hash(txt).should == {:unadded=>[["modified", "a.txt"], ["modified", "committed.txt"], ["deleted", "deleteme.txt"]], :untracked=>[], :added=>[["new file", "a.txt"]]}
  end

  it "Populates added keys" do
    txt = "
      AM a.txt
      A  d/d.txt
      R  rename.txt -> renamed.txt
    ".unindent
    Git.status_to_hash(txt)[:added].should == [["new file", "a.txt"], ["new file", "d/d.txt"], ["renamed", "rename.txt -> renamed.txt"]]
  end

end



__END__

# Old test before moving to rspec:

#require 'test/unit'
#$:.unshift "../"
require 'ol'
require 'core_ext'
require 'yaml'
require 'git'

class GitTest < Test::Unit::TestCase

  @@status_raw = '
    # On branch master
    # Changes to be committed:
    #   (use "git reset HEAD <file>..." to unstage)
    #
    #	new file:   unadded.txt
    #	modified:   modified.rb
    #
    # Changed but not updated:
    #   (use "git add <file>..." to update what will be committed)
    #
    #	modified:   changed.rb
    #	modified:   modifiedandadded.rb
    #
    # Untracked files:
    #   (use "git add <file>..." to include in what will be committed)
    #
    #	untracked.rb
    #	untracked2.rb
    '.unindent

  @@status_parsed = '
    | On branch master
    | Changes to be committed:
    |   (use "git reset HEAD <file>..." to unstage)
    - new file: unadded.txt
    - modified: modified.rb
    | Changed but not updated:
    |   (use "git add <file>..." to update what will be committed)
    - modified: changed.rb
    - modified: modifiedandadded.rb
    | Untracked files:
    |   (use "git add <file>..." to include in what will be committed)
    - untracked.rb
    - untracked2.rb
    '.unindent

  @@status_parsed_only_new = '
    | On branch master
    | Initial commit
    | Untracked files:
    |   (use "git add <file>..." to include in what will be committed)
    - a.txt
    nothing added to commit but untracked files present (use "git add" to track)
    '.unindent

  # Should parse status initially
  def test_status_internal
    result = Git.status_internal @@status_raw
    assert_equal @@status_parsed, result
  end

  # Should parse status into hash of arrays
  def test_status_to_hash

    # Parse it
    hash = Git.status_to_hash @@status_parsed

    assert hash[:unadded]
    assert_equal [
      ['modified', 'changed.rb'],
      ['modified', 'modifiedandadded.rb']],
      hash[:unadded]

    assert hash[:added]
    assert_equal [
      ['new file', 'unadded.txt'],
      ['modified', 'modified.rb']],
      hash[:added]

    assert hash[:untracked]
    assert_equal [
      ['untracked', 'untracked.rb'],
      ['untracked', 'untracked2.rb']],
      hash[:untracked]
  end

  # Should handle only new
  def test_status_to_hash_only_new
    # Parse it
    hash = Git.status_to_hash @@status_parsed_only_new

    assert hash[:untracked]
  end

end
