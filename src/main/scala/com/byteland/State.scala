package com.byteland

sealed trait State {
  def hasRoute[T](b: T): Boolean
  def unify[T](b: T): Option[State]
}

case class City(id: Int, connected: Set[City] = Set.empty[City]) extends State { self ⇒

  def isConnected(that: City): Boolean = that.connected.contains(self) || self.connected.contains(that)

  override def hasRoute[T](b: T): Boolean = b match {
    case c: City         ⇒ isConnected(c)
    case u: UnifiedState ⇒ u.states.exists(isConnected)
    case _               ⇒ false
  }

  override def unify[T](b: T): Option[State] = b match {
    case c: City         ⇒ if (hasRoute(c)) Some(UnifiedState(Set(self, c))) else None
    case u: UnifiedState ⇒ if (hasRoute(u)) Some(UnifiedState(u.states + self)) else None
    case _               ⇒ None
  }
}

case class UnifiedState(states: Set[City]) extends State { self ⇒

  override def hasRoute[T](b: T): Boolean = b match {
    case c: City         ⇒ c.hasRoute(self)
    case u: UnifiedState ⇒ states.exists(_.hasRoute(u))
    case _               ⇒ false
  }

  override def unify[T](b: T): Option[State] = b match {
    case c: City         ⇒ if (hasRoute(c)) Some(UnifiedState(self.states + c)) else None
    case u: UnifiedState ⇒ if (hasRoute(u)) Some(UnifiedState(self.states ++ u.states)) else None
    case _               ⇒ None
  }
}
