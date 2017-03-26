// Package reporter provides Ginkgo custom reporter for Codewars.
package reporter

import (
	"fmt"
	"strings"

	"github.com/onsi/ginkgo/config"
	"github.com/onsi/ginkgo/types"
)

type cwReporter struct {
	// keep track of nested describes using map
	specs map[int]string
}

func New() *cwReporter {
	return &cwReporter{
		specs: make(map[int]string),
	}
}

func (r *cwReporter) SpecSuiteWillBegin(config config.GinkgoConfigType, summary *types.SuiteSummary) {
}

func (r *cwReporter) BeforeSuiteDidRun(setupSummary *types.SetupSummary) {
}

func (r *cwReporter) AfterSuiteDidRun(setupSummary *types.SetupSummary) {
}

func (r *cwReporter) SpecWillRun(spec *types.SpecSummary) {
	n := len(spec.ComponentTexts)
	for i := 1; i+1 < n; i++ {
		v := spec.ComponentTexts[i]
		// append LineNumer to support multiple test cases with same name
		id := fmt.Sprintf("%s:%d", v, spec.ComponentCodeLocations[i].LineNumber)
		s, ok := r.specs[i]
		if !ok || s != id {
			if ok {
				m := len(r.specs)
				for j := i; j <= m; j++ {
					fmt.Println("\n<COMPLETEDIN::>")
					delete(r.specs, j)
				}
			}
			r.specs[i] = id
			fmt.Printf("\n<DESCRIBE::>%s\n", escape(v))
		}
	}
	fmt.Printf("\n<IT::>%s\n", escape(spec.ComponentTexts[n-1]))
}

func (r *cwReporter) SpecDidComplete(spec *types.SpecSummary) {
	switch spec.State {
	case types.SpecStatePassed:
		fmt.Println("\n<PASSED::>Test Passed")
	case types.SpecStateFailed:
		fmt.Println("\n<FAILED::>Test Failed")
		fmt.Printf("\n<LOG:ESC:>%s\n", escape(spec.Failure.Message))
	case types.SpecStatePanicked:
		fmt.Printf("\n<ERROR::>%s\n", escape(spec.Failure.Message))
		fmt.Printf("\n<LOG::Panic>%s\n", escape(spec.Failure.ForwardedPanic))
		fmt.Printf("\n<TAB::Stack Trace>%s\n", escape(spec.Failure.Location.FullStackTrace))
	case types.SpecStateTimedOut:
	case types.SpecStateSkipped:
	case types.SpecStatePending:
	}
	fmt.Printf("\n<COMPLETEDIN::>%.4f\n", spec.RunTime.Seconds()*1000)
}

func (r *cwReporter) SpecSuiteDidEnd(summary *types.SuiteSummary) {
	m := len(r.specs)
	for j := 1; j <= m; j++ {
		fmt.Println("\n<COMPLETEDIN::>")
	}
}

func escape(s string) string {
	return strings.Replace(s, "\n", "<:LF:>", -1)
}
