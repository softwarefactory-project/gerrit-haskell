# Changelog

## 0.1.6.1

- Apply hlint suggestion
- Add version bounds to the build-depends

## 0.1.6.0

- Add a nix flake
- Add the work_in_progress change field support
- Remove invalid change status "DRAFT"

## 0.1.5.1

- Allow bytestring-0.11

## 0.1.5.0

- Add missing attributes for the Event.Change
- Add getClientWithManager to support custom Manager

## 0.1.4.0

- Set GerritAuthor name field as optional
- Allow aeson-2.0

## 0.1.3.0 (2021-10.21)

- Honor the authentication for get request

## 0.1.2.0 (2021-10-18)

- Add missing Eq and Show instances
- Add support for change' fields: topic and hashtags

## 0.1.1.0 (2021-10-13)

- Add EventCreatedOn Event attribute
- Split Data module into specialized modules
- Support more fields for GerritChange
- Add the getProjects function
- withClient takes optional credentials

## 0.1.0.0 (2021-05-30)

- Basic Gerrit API data types.
