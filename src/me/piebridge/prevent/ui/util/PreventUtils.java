package me.piebridge.prevent.ui.util;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;

import me.piebridge.prevent.common.PreventIntent;

/**
 * Created by thom on 15/7/13.
 */
public class PreventUtils {

    private PreventUtils() {

    }

    public static void update(Context context, String[] packages, boolean add) {
        Intent intent = new Intent(PreventIntent.ACTION_UPDATE_PREVENT, Uri.fromParts(PreventIntent.SCHEME, context.getPackageName(), null));
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY);
        intent.putExtra(PreventIntent.EXTRA_PACKAGES, packages);
        intent.putExtra(PreventIntent.EXTRA_PREVENT, add);
        context.sendBroadcast(intent);
    }

}
