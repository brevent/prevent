package com.android.server.am;

import android.os.IBinder;

/**
 * Created by thom on 2016/10/15.
 */
class ActivityRecord {
    String packageName;

    static ActivityRecord forTokenLocked(IBinder token) {
        throw new UnsupportedOperationException();
    }

    static ActivityRecord forToken(IBinder token) {
        throw new UnsupportedOperationException();
    }
}
