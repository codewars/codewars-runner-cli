class CodewarsListener: public TestListener
{
	virtual void TestRunStarting() {}
	virtual void TestRunEnded(const TestResults&) {}
	virtual void ContextRunStarting(const ContextBase& ctx) {
		std::cout << "\n<DESCRIBE::>" << ctx.Name() << std::endl;
	}
	virtual void ContextRunEnded(const ContextBase&) {
		std::cout << "\n<COMPLETEDIN::>" << std::endl;
	}
	virtual void SpecRunStarting(const ContextBase& ctx, const std::string& specName) {
		std::cout << "\n<IT::>" <<  specName << std::endl;
	}
	virtual void SpecSucceeded(const ContextBase&, const std::string& specName) {
		std::cout << "\n<PASSED::>" << "Test Passed" << std::endl << "\n<COMPLETEDIN::>" << std::endl;
	}
	virtual void SpecFailed(const ContextBase&, const std::string& specName, const FailedTestResult& result) {
		std::cout << "\n<FAILED::>" << format(result.GetErrorMessage()) << std::endl << "\n<COMPLETEDIN::>" << std::endl;
	}

	std::string format(const std::string &str) {
		std::string new_line = "\n";
		std::string line_feed = "<:LF:>";
		std::string new_string = str;
		size_t start_pos = 0;
		while ((start_pos = new_string.find(new_line, start_pos)) != std::string::npos) {
			new_string.replace(start_pos, new_line.length(), line_feed);
			start_pos += line_feed.length();
		}
		return new_string;
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
