#!/bin/bash

set -e

curl http://localhost:8080/
curl http://localhost:8080/cats
curl http://localhost:8080/comments/cats

curl --form "content=I love foo" http://localhost:8080/foo
curl --form "comment=Me too" http://localhost:8080/comments/foo

curl http://localhost:8080/comments/foo
curl http://localhost:8080/

curl -X DELETE http://localhost:8080/foo
curl http://localhost:8080/
