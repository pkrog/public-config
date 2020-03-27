// vi: ft=objc

NSSound *sound = [[NSSound alloc] initWithContentsOfFile:[[NSBundle mainBundle] pathForResource:@"mySound" ofType:@"mp3"] byReference:NO];
[sound play];
[sound release];
