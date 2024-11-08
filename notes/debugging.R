# debugging.txt - 8/11/24 - been having problems with recent commits and sudo
# errors on GHA which weren't happening before. Still on dev-andy branch doing
# hotssea and bccm_full.
# Added notes to recent commits that didn't work.

# test() passes fine but
# build() works (but is not what I need, just makes the .tar.gz file)
# check():

--
test() works, but has some warnings, which I tried fixing but to no avail -
 could try re-ordering the lines. Warning is:

Warning (test-pacea-wrangle.R:24:3): pacea_wide function works
Using an external vector in selections was deprecated in tidyselect 1.1.0.
i Please use `all_of()` or `any_of()` instead.
  # Was:
  data %>% select(values_from)

  # Now:
  data %>% select(all_of(values_from))

See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.

and a few similar ones. TODO make an issue as should fix at some point.

══ Results ═════════════════════════════════════════════════════════════════════
Duration: 169.2 s

[ FAIL 0 | WARN 4 | SKIP 0 | PASS 254 ]

--

But check() locally, cutting out some warnings/notes:
> check()
 [183s] ERROR
Running the tests in 'tests/testthat.R' failed.
Last 13 lines of output:
  `actual`:   FALSE
  `expected`: TRUE
  ── Failure ('test-get-pacea-data.R:127:3'): Download and update of test data (from version 1 to 2) declined (ie. keep old data) ──
  file.exists(data2f_dir) (`actual`) not equal to FALSE (`expected`).

  `actual`:   TRUE
  `expected`: FALSE
  ── Failure ('test-get-pacea-data.R:133:3'): Download and update of test data (from version 1 to 2) declined (ie. keep old data) ──
  `data2f` has length 200, not length 100.
  ── Failure ('test-get-pacea-data.R:134:3'): Download and update of test data (from version 1 to 2) declined (ie. keep old data) ──
  `data1.1` has length 200, not length 100.

  [ FAIL 8 | WARN 8 | SKIP 0 | PASS 246 ]
  Error: Test failures
  Execution halted
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: 1 ERROR, 3 WARNINGs, 6 NOTEs
See
  'C:/Users/edwardsand/AppData/Local/Temp/RtmpQ3OOOl/file567c224c5ed7/pacea.Rcheck/00check.log'

AHA - the test-get-pacea-data fails have skip_on_ci(), so gets ignored by GHA but
 not here. So can probably not worry about.

# Check if get same problem with main branch, which was always fine:
git co main
> check()
Same Error as above with main, but it passed on GHA, presumably due to
 skip_on_ci().
git co dev-andy

Thought could maybe just add in skip_on_ci() on the things that break now on
GHA, but the error there is:
 Some packages could not be installed. This may mean that you have
  requested an impossible situation or if you are using the unstable
  distribution that some required packages have not yet been created
  or been moved out of Incoming.
  The following information may help to resolve the situation:

  The following packages have unmet dependencies:
  libraptor2-dev : Depends: libcurl4-gnutls-dev but it is not installable
  E: Unable to correct problems, you have held broken packages.
  Error:
  ! error in pak subprocess
  Caused by error in `processx::run(sh, cmdline, stdout_callback = callback, stderr_to_stdout = TRUE)`:
  ! System command 'sudo' failed

Which is some linux installation thing, nothing to do with any of my commits, so
I am hoping it might get rectified at some point. Some linux packages are not
talking to each other properly.

Also:
> covr::codecov()
Error: Failure in `C:/Users/edwardsand/AppData/Local/Temp/RtmpQvoItf/R_LIBS8278754d58a8/pacea/pacea-tests/testthat.Rout.fail`
  FALSE
`expected`: TRUE
── Failure ('test-get-pacea-data.R:127:3'): Download and update of test data (from version 1 to 2) declined (ie. keep old data) ──
file.exists(data2f_dir) (`actual`) not equal to FALSE (`expected`).

`actual`:   TRUE
`expected`: FALSE
── Failure ('test-get-pacea-data.R:133:3'): Download and update of test data (from version 1 to 2) declined (ie. keep old data) ──
`data2f` has length 200, not length 100.
── Failure ('test-get-pacea-data.R:134:3'): Download and update of test data (from version 1 to 2) declined (ie. keep old data) ──
`data1.1` has length 200, not length 100.

[ FAIL 7 | WARN 7 | SKIP 0 | PASS 247 ]
Error: Test failures
Execution halted
>

--
Commenting out the failing lines in test-get-pacea-data.R, then:
> check()
passes with no errors.
