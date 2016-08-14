require "./formatter.cr"

describe "Foo" do
  it "should do something" do
    "a".should eq "a"
  end

  context "some other context" do
    it "should do something else" do
      "b".should eq "a"
    end
  end
end