require 'minitest/autorun'
require 'ostruct'
 
class TestLiterateMD < Minitest::Test
  def run_and_verify opts, actual_file, expected_file
    tidy_up actual_file
    $opts = opts
    run_it
    expected = File.read expected_file
    actual = File.read actual_file
    assert_equal expected, actual
    tidy_up actual_file
  end

  def test_tangle
    run_and_verify OpenStruct.new(
      :tangle => true, 
      :weave => false, 
      :outputdir => '.',
      :files => 'test/test.md',
      :lang => 'ruby'), 'test/test.rb', 'test/expected_test.rb'
  end
 
  def test_weave
    run_and_verify OpenStruct.new(
      :tangle => false, 
      :weave => true, 
      :standalone => true,
      :outputdir => '.',
      :files => 'test/test.md',
      :lang => 'ruby'), 'test.md.html', 'test/expected_test.html'
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