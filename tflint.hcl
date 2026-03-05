/*
Original path to file: ~/.tflint.hcl
Configuration for tflint terraform linter

Created on: Wed 04 Mar 2026
Last Updated: Wed 04 Mar 2026
*/

plugin "terraform" {
  enabled = true
  version = "0.13.0"
  source  = "github.com/terraform-linters/tflint-ruleset-terraform"
}

plugin "google" {
  enabled = true
  version = "0.38.0"
  source  = "github.com/terraform-linters/tflint-ruleset-google"
}

plugin "aws" {
  enabled = true
  version = "0.45.0"
  source = "github.com/terraform-linters/tflint-ruleset-aws"
}

plugin "azurerm" {
  enabled = true
  version = "0.31.1"
  source = "github.com/terraform-linters/tflint-ruleset-azurerm"
}
