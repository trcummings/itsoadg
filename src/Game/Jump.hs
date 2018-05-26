-- This module tracks the possible states and state transitions of the Jump component

module Game.Jump where

import Game.Types (Jump(..))

-- F F F
floating :: Jump
floating = Jump
  { isGrounded = False
  , isJumping = False
  , buttonPressed = False }

-- F F T
impossibleJumpState :: Jump
impossibleJumpState = Jump
  { isGrounded = False
  , isJumping = False
  , buttonPressed = True }

-- F T F
falling :: Jump
falling = Jump
  { isGrounded = False
  , isJumping = True
  , buttonPressed = False }

-- T F F
onGround :: Jump
onGround = Jump
  { isGrounded = True
  , isJumping = False
  , buttonPressed = False }

-- T F T
jumpRequested :: Jump
jumpRequested = Jump
  { isGrounded = True
  , isJumping = False
  , buttonPressed = True }

-- F T T
jumping :: Jump
jumping = Jump
  { isGrounded = False
  , isJumping = True
  , buttonPressed = True }

-- T T T
landed :: Jump
landed = Jump
  { isGrounded = True
  , isJumping = True
  , buttonPressed = True }
