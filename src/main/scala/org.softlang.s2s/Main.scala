package org.softlang.s2s

import org.softlang.s2s.main.Compare
import org.softlang.s2s.main.Generator
import org.softlang.s2s.main.Dev
import org.softlang.s2s.main.Profile
import org.softlang.s2s.main.S2S
import org.softlang.s2s.main.Server

@main def s2s(args: String*): Unit = S2S.run(args)

@main def compare(): Unit = Compare.run()

@main def profile(): Unit = Profile.run()

@main def dev(): Unit = Dev.run()

@main def serve(): Unit = Server.main(Array())

@main def gen(): Unit = Generator.run(samples_x8 = 6250, debug = false)
