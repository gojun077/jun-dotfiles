# default path to file is ~/.tflint.hcl
config {
  format = "compact"
  plugin_dir = "~/.tflint.d/plugins"
}

plugin "google" {
  enabled = true
  version = "0.29.0"
  source = "github.com/terraform-linters/tflint-ruleset-google"
}

plugin "terraform" {
  enabled = true
  preset = "recommended"
}
