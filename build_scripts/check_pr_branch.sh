# Runs elm package diff, checks Travis CI for PR info and determines if PR is on correct branch
# Input parameters:
#   First argument: The current branch (reported as $TRAVIS_BRANCH on Travis CI environment variables)
#   Second argument: string to verify as the major version branch
#   Third argument: Input should be Travis CI environment variable ($TRAVIS_PULL_REQUEST). The pull request number if the current job is a pull request, “false” if it’s not a pull request.

# Usage for local testing: check_pr_branch my_branch_name master false
# Usage for use in Travis CI: check_pr_branch $TRAVIS_BRANCH master $TRAVIS_PULL_REQUEST

#User message when MAJOR version on minor branch
read -r -d '' wrong_branch_major_minor << EOM
WRONG BRANCH — submit this PR to master instead.

You have submitted your PR to the v8 branch, which is only for 8.x.x
releases of elm-mdl. Your PR compiles just fine, but the changes you
introduce force a major version bump of elm-mdl, and so cannot be
released as a 8.x.x version.

Please re-submit your PR against master.

Apologies for the inconvenience,

The elm-mdl build team :)
EOM


# User message when MINOR/PATCH version on major branch
read -r -d '' wrong_branch_minor_major << EOM
WRONG BRANCH — submit this PR to v8 instead.

You have submitted your PR to the master branch, which is only for 9.x.x
releases of elm-mdl. Your changes do not require a major version bump
and can be released as a 8.x.x version.

Please re-submit your PR against v8.

Apologies for the inconvenience,

The elm-mdl build team :)
EOM


current_branch=$1
major_version_branch=$2
travis_pull_request=$3


if [ $travis_pull_request != "false" ]; then
  # Is a pull request - need to check elm-package diff
  string=$(libsysconfcpus/bin/sysconfcpus -n 2 elm package diff)
  if [[ $? != 0 ]]; then
      echo "Command failed."
      exit -1
  elif [[ $string == *"This is a MAJOR change."* ]]; then
      # Pull request and MAJOR change. Need to check if the branch reported by Travis is equal to the "official" master branch
      # Check if current branch is other than master
      if [ $current_branch == $major_version_branch ]; then
        # Allowed. This is a Major change into the major versjon branch.
        exit 0
      else
        echo "$wrong_branch_major_minor"
        exit 1
      fi
  else
      # Pull request and MINOR/PATCH change - Check that it is not on MASTER
      # Check if current branch is other than master
      if [ $current_branch == $major_version_branch ]; then
        echo "$wrong_branch_minor_major"
        exit 1
      else
        # Allowed. This is a minor/patch to a valid branch
        exit 0
      fi
  fi
fi
