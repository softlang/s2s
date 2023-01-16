package org.softlang.s2s

import org.softlang.s2s.main.Compare
import org.softlang.s2s.main.Dev
import org.softlang.s2s.main.Profile
import org.softlang.s2s.main.S2s

@main def s2s(args: String*): Unit = S2s.run(args)

@main def compare(): Unit = Compare.run()

@main def profile(): Unit = Profile.run()

@main def dev(): Unit = Dev.run()
