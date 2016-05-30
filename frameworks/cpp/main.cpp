
class CodewarsListener: public TestListener 
{
	virtual void TestRunStarting() {}
	virtual void TestRunEnded(const TestResults&) {}
	virtual void ContextRunStarting(const ContextBase& ctx) {
		std::cout << "<DESCRIBE::>" << ctx.Name() << std::endl;
	}
	virtual void ContextRunEnded(const ContextBase&) {
		std::cout << "<COMPLETEDIN::>" << std::endl;

	}
	virtual void SpecRunStarting(const ContextBase& ctx, const std::string&) {
		std::cout << "<DESCRIBE::>" <<  ctx.Name() << std::endl;

	}
	virtual void SpecSucceeded(const ContextBase&, const std::string& str) {
		std::cout << "<PASSED::>" << str << std::endl;

	}
	virtual void SpecFailed(const ContextBase&, const std::string& str) {
		std::cout << "<FAILED::>" << str << std::endl;
	}
};

int main(int argc, const char *argv[])
{
	DefaultTestResultsOutput output;
	TestRunner runner(output);

	CodewarsListener listener;
	runner.AddListener(&listener);

	runner.Run();
}
