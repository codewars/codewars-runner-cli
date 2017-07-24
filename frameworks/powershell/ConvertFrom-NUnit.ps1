<#
.SYNOPSIS
    Takes an NUnit XML output from Pester and generates the output required by Codewars.
#>
# function ConvertFrom-NUnitXML {
[CmdletBinding()]
[OutputType([string])]
param(
    [Parameter(Mandatory = $true)]
    [ValidateScript( {Test-Path -Path $_ })]
    [string]
    $File
)

begin {

    [xml]$NUnitXML = (Get-Content $File)
    $Output = ''

}
process {

    $TestResults = $NUnitXML.SelectNodes("test-results/test-suite/results/*")
    $TestResults | ForEach-Object {

        $Output += "<DESCRIBE::>$($_.Name)`n"
        $_.SelectNodes("results/*") | ForEach-Object {
            $Output += "<IT::>$($_.name)`n"

            if ( $_.success -eq 'True') {
                $Output += "<PASSED::>Test Passed`n"
            }
            else {
                $Message = $_.failure.message -replace "`n", "<:LF:>"
                $Output += "<FAILED::>$Message`n"
            }

            $Output += "<COMPLETEDIN::>$([math]::round(1000 * $_.time))`n"
        }

        $Output += "<COMPLETEDIN::>$([math]::round(1000 * $_.time))`n"
    }

}
end { $Output }
# }


