/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

/**
  * Factor was renamed to Representation in version 1.11.0.
  * This type alias lets existing code compile; use Representation instead.
  */
@deprecated("Use Representation", since = "1.11.0")
type Factor = Representation

/**
  * Factor was renamed to Representation in version 1.11.0.
  * This alias for the companion object lets existing code compile; use Representation instead.
  */
@deprecated("Use Representation", since = "1.11.0")
val Factor: Representation.type = Representation
