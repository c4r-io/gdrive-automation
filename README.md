
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdrive-automation

<!-- badges: start -->
<!-- badges: end -->

The goal of gdrive-automation is to automate processing and tasks
related to the C4R Shared Drive, to aid in synchronization of states and
content.

Specific actions include: \* check for updates in Unit Roadmaps and
process accordingly (send notifications, update files) \* check for
updates in the Unit Tracking spreadsheet and process accordingly (send
notifications, update files)

# Data Model

Data about a Unit and its Status is shared across two distinct files: \*
the Unit Roadmap (“roadmap”) specific to the unit - a google doc \* the
overall Unit Tracking spreadsheet (“tracker”) - a google sheet

The data that is unique in the roadmap is: \* metadata about the unit
(title, description, mini-units, activities)

## Resolving Status Updates

Both the roadmap and the tracker will contain status indicators for
various components / phases of a unit. (The technical limitation here is
embedding in the google doc an interactive interface for data that is
stored elsewhere.)

There are 4 states for the status, which generally progresses from: “Not
started” -\> “Submitted” -\> “Under review” -\> “Approved”

The below documentation clarifies how to resolve situations where the
status for a particular item is different in the roadmap vs. the
tracker.

### Case 0. Identical Status

No action needed.

### Case 1. Tracker is “further along” than the Roadmap

1.  Update the roadmap to match.
2.  Log the change.

### Case 2. Roadmap is “further along” than the Tracker

#### Case 2a. “Not started” -\> “Submitted”

1.  Update tracker to match.
2.  Send notifications as necessary.
3.  Log the change.

#### Case 2b. “Submitted” -\> “Under review”

1.  Update tracker to match.
2.  Send notifications as necessary.
3.  Log the change.

#### Case 2c. {anything} -\> “Approved”

(if change is for item that is for METER to review) 1. Update tracker to
match. 2. Send notifications as necessary. 3. Log the change.

#### Case 2d. Anything else.

1.  Revert the roadmap to match the tracker.
2.  Notify Hao of the discrepancy.
3.  Log the change.
