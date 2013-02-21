require 'test/unit'
require 'ostruct'
 
class TestLiterateMD < Test::Unit::TestCase
  def test_tangle
    tidy_up(actual_file = 'test/test.rb')
    $opts = OpenStruct.new(
      :tangle => true, 
      :weave => false, 
      :outputdir => '.',
      :files => 'test/test.md',
      :lang => 'ruby')
    run_it
    expected = File.open('test/expected_test.rb', 'r'){|f|f.read}
    actual = File.open(actual_file, 'r'){|f|f.read}
    assert_equal expected, actual
    tidy_up actual_file
  end
 
  def test_weave
    # TODO!
  end
  
  def tidy_up file
    File.delete file if File.exists? file
  end
  
  def run_it 
    begin
  	  load 'bin/literate_md'
	rescue SystemExit => e
      assert_equal e.status, 0
	end
  end
end