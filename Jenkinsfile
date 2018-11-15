def HTTP_PROXY = "http://www-proxy:8080"
def HTTPS_PROXY = "http://www-proxy:8080"
def NO_PROXY = "localhost,127.0.0.1,.otp.ericsson.se,otp.ericsson.se,otp,erlang,support,nessie,wiki,monitor,jenkins"

def DOCKER_BUILD_ARGS = "--build-arg MAKEFLAGS=-j16 --build-arg https_proxy=${HTTP_PROXY} --build-arg http_proxy=${HTTP_PROXY} --build-arg HTTP_PROXY=${HTTP_PROXY} --build-arg HTTPS_PROXY=${HTTP_PROXY} --build-arg NO_PROXY=${NO_PROXY} --build-arg no_proxy=${NO_PROXY} ."

def docker_image

pipeline {
  environment { 
    OTP_STRICT_INSTALL = 'yes'
  }
  agent any
  stages {
    stage('docker_build') {
      steps {
         script {
           docker.build("erlang/ubuntu-build:64bit","-f scripts/Dockerfile.64.ubuntu --pull ${DOCKER_BUILD_ARGS}")
         }
      }
    }
    stage('build') {
      steps {
        script {
          sh 'git archive --format=tar.gz --prefix=otp_src/ -o otp.tar.gz HEAD'
//           sh 'git archive --format=tar --prefix=otp_src/ HEAD | tar xf - && tar czf otp.tar.gz --mtime=\'1970-01-01\' otp'
          docker_image = docker.build("erlang:${GIT_BRANCH}","-f scripts/Dockerfile.64 ${DOCKER_BUILD_ARGS}")
//          scanForIssues tool: [$class: 'C']
        }
        step([$class: 'WarningsPublisher', canComputeNew: false, canResolveRelativePaths: false, defaultEncoding: '', excludePattern: '', healthy: '', includePattern: '', messagesPattern: '', parserConfigurations: [[parserName: 'GNU Make + GNU C Compiler (gcc)', pattern: 'error_and_warnings.txt']], unHealthy: ''])
      }
      
    }
    stage('build next') {
      parallel {
        stage('special builds') {
          steps {
            script {
              docker_image.inside {
                sh 'cd $ERL_TOP/erts/emulator && make smp TYPE=debug'
                sh 'cd $ERL_TOP/erts/emulator && make lcnt'
                sh 'cd $ERL_TOP/erts/emulator && make lcnt TYPE=debug'
                sh 'cd $ERL_TOP/erts/emulator && make plain'
                sh 'cd $ERL_TOP/erts/emulator && make plain TYPE=debug'
              }
            }
          }
        }
        stage('test') {
          steps {
             script {
               docker_image.inside {
                 sh 'cd $ERL_TOP && ./otp_build tests'
                 sh 'cd $ERL_TOP/release/tests/ && $ERL_TOP/scripts/build-otp-tests'
               }
             }
          }
        }
        stage('docs') {
          steps {
            script {
              docker_image.inside {
                sh 'cd $ERL_TOP && make release_docs'
                sh '(cd $ERL_TOP/system/doc/ && make release_docs TESTROOT=$TARGET_DIR INSTALLROOT=$TARGET_DIR)'
                sh 'cd $ERL_TOP && make xmllint'
              }
            }
          }
        }
      }
    }
  }
  post {
    always {
      recordIssues enabledForFailure: true, tools: [[tool: [$class: 'CheckStyle']]]
    }
  }
}