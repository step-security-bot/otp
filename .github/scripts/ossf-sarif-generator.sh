#!/bin/bash

flag=$1

cat <<EOF
{
  "version": "2.1.0",
  "\$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/main/sarif-2.1/schema/sarif-schema-2.1.0.json",
  "runs": [
    {
      "tool": {
        "driver": {
          "informationUri": "https://github.com/erlang/otp/.github/workflow/ossf-scanner",
          "name": "ossf-scanner",
          "rules": [
            {
              "id": "1",
              "name": "Missing Compiler Flag",
              "shortDescription": {
                "text": "Missing CFLAGS $flag"
              },
              "fullDescription": {
                "text": "A OSSF C/C++ compiler hardening flag is missing from the tests. Please check https://best.openssf.org/Compiler-Hardening-Guides/Compiler-Options-Hardening-Guide-for-C-and-C++ for details."
              }
            }
          ],
          "version": "1.0"
        }
      },
      "artifacts": [
        {
          "location": {
            "uri": ".github/workflow/main/ossf-scanner.yaml"
          },
          "length": -1
        }
      ],
      "results": [
        {
          "ruleId": "1",
          "ruleIndex": 0,
          "level": "warning",
          "message": {
            "text": "Missing CFLAGS $flag"
          },
          "locations": [
            {
              "physicalLocation": {
                "artifactLocation": {
                  "uri": ".github/workflow/main/ossf-scanner.yaml"
                }
              }
            }
          ]
        }
      ]
    }
  ]
}
EOF