# default path to file is ~/.tflint.hcl
config {
  format = "compact"
  plugin_dir = "~/.tflint.d/plugins"

  module = true
  force = false
  disabled_by_default = false
}

plugin "google" {
  enabled = true
  version = "0.22.2"
  source = "github.com/terraform-linters/tflint-ruleset-google"
}

plugin "terraform" {
  enabled = true
  preset = "recommended"
}
