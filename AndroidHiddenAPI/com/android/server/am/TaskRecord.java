package com.android.server.am;

import android.content.Intent;

/**
 * Created by thom on 2016/10/23.
 */
final class TaskRecord {

    Intent intent;

    Intent affinityIntent;

    Intent getBaseIntent() {
        throw new UnsupportedOperationException();
    }

}
