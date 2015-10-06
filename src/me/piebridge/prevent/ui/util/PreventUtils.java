package me.piebridge.prevent.ui.util;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.widget.Toast;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import me.piebridge.forcestopgb.R;

import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/7/13.
 */
public class PreventUtils {

    private PreventUtils() {

    }

    public static void update(Context context, String[] packages, boolean add) {
        if (packages == null || packages.length == 0) {
            return;
        }
        Intent intent = new Intent(PreventIntent.ACTION_UPDATE_PREVENT, Uri.fromParts(PreventIntent.SCHEME, context.getPackageName(), null));
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY);
        intent.putExtra(PreventIntent.EXTRA_PACKAGES, packages);
        intent.putExtra(PreventIntent.EXTRA_PREVENT, add);
        context.sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, new PreventListReceiver(), null, 0, null, null);
    }

    public static boolean updateTimeout(Context context, String value) {
        long timeout;
        try {
            timeout = Long.valueOf(value);
        } catch (NumberFormatException e) {
            UILog.d(value + " is not long", e);
            return false;
        }
        Intent intent = new Intent(PreventIntent.ACTION_UPDATE_TIMEOUT, Uri.fromParts(PreventIntent.SCHEME, context.getPackageName(), null));
        intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY);
        intent.putExtra(PreventIntent.EXTRA_TIMEOUT, timeout);
        context.sendBroadcast(intent, PreventIntent.PERMISSION_SYSTEM);
        return true;
    }

    private static class PreventListReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();
            String result = getResultData();
            if (PreventIntent.ACTION_UPDATE_PREVENT.equals(action) && result != null) {
                handlePackages(context, result);
            }
        }

        private void handlePackages(Context context, String result) {
            try {
                JSONObject json = new JSONObject(result);
                Set<String> prevents = new TreeSet<String>();
                Iterator<String> it = json.keys();
                while (it.hasNext()) {
                    String key = it.next();
                    prevents.add(key);
                }
                int size = getResultCode();
                if (prevents.size() == size) {
                    UILog.i("update prevents: " + prevents.size());
                    PreventListUtils.save(context, prevents);
                    String message = context.getString(R.string.updated_prevents, prevents.size());
                    Toast.makeText(context, message, Toast.LENGTH_SHORT).show();
                } else {
                    UILog.e("update prevents: " + prevents.size() + " != " + size);
                }
            } catch (JSONException e) {
                UILog.e("cannot convert to json", e);
            }
        }
    }

}
