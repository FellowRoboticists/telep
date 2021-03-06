#!/bin/bash

# Use this script to generate the public/private keys for a robot
# to register with the telep server.
#
# The keys will be generated in the current directory.

ROBOT_NAME=$1
shift 1

# Make sure we have a robot name; otherwise, what's the point?
[ -z "$ROBOT_NAME" ] && echo "Must specify the robot name" && exit 1

PRIVATE_KEY_NAME="${ROBOT_NAME}_private.pem"
ANDROID_PRIVATE_KEY_NAME="${ROBOT_NAME}_private_android.pem"
PUBLIC_KEY_NAME="${ROBOT_NAME}_public.pem"

echo "The name of the robot is: $ROBOT_NAME"

# Generate the private key
openssl genrsa -out $PRIVATE_KEY_NAME 2048

# Generate the public key
openssl rsa -in $PRIVATE_KEY_NAME -pubout -out $PUBLIC_KEY_NAME

# For the coup de gras, we also need to generate a key from the
# robot key that Android can deal with
openssl pkcs8 -topk8 -inform PEM -outform PEM -in $PRIVATE_KEY_NAME -out $ANDROID_PRIVATE_KEY_NAME -nocrypt
