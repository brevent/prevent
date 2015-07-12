package me.piebridge.forcestopgb.ui;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;

import me.piebridge.forcestopgb.common.CommonIntent;

/**
 * Created by thom on 15/7/13.
 */
public class PreventUtils {

    private PreventUtils() {

    }

    static void update(Context context, String[] packages, boolean add) {
        Intent intent = new Intent(CommonIntent.ACTION_UPDATE_PREVENT, Uri.fromParts("package", context.getPackageName(), null));
        intent.putExtra(CommonIntent.EXTRA_PACKAGES, packages);
        intent.putExtra(CommonIntent.EXTRA_PREVENT, add);
        context.sendBroadcast(intent);
    }

    static void add(Context context, String[] packages) {
        update(context, packages, true);

    }

    static void remove(Context context, String[] packages) {
        update(context, packages, false);
    }


}
