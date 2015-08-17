package me.piebridge.prevent.framework.util;

import android.appwidget.AppWidgetManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by thom on 15/7/31.
 */
public class WidgetUtils {

    private static final Object LOCK = new Object();
    private static Map<String, Set<ComponentName>> widgets = new HashMap<String, Set<ComponentName>>();

    private WidgetUtils() {

    }

    public static void onPackageChanged(String packageName) {
        if (packageName != null) {
            synchronized (LOCK) {
                widgets.remove(packageName);
            }
        }
    }

    public static boolean addWidget(ComponentName cn) {
        String packageName = cn.getPackageName();
        if (packageName == null) {
            return false;
        }
        Set<ComponentName> components = widgets.get(packageName);
        if (components == null) {
            synchronized (LOCK) {
                widgets.put(packageName, new HashSet<ComponentName>());
                components = widgets.get(packageName);
            }
        }
        components.add(cn);
        return true;
    }

    public static boolean isWidget(ComponentName cn) {
        String packageName = cn.getPackageName();
        if (packageName == null) {
            return false;
        }
        Set<ComponentName> components = widgets.get(packageName);
        return components != null && components.contains(cn);
    }

    public static boolean isWidget(Context context, ComponentName cn) {
        if (isWidget(cn)) {
            return true;
        }
        PackageManager packageManager = context.getPackageManager();
        Intent intent = new Intent(AppWidgetManager.ACTION_APPWIDGET_UPDATE);
        intent.setPackage(cn.getPackageName());
        List<ResolveInfo> broadcastReceivers = packageManager.queryBroadcastReceivers(intent, 0);
        final int size = broadcastReceivers == null ? 0 : broadcastReceivers.size();
        for (int i = 0; i < size; ++i) {
            ActivityInfo ai = broadcastReceivers.get(i).activityInfo;
            if ((ai.applicationInfo.flags & ApplicationInfo.FLAG_EXTERNAL_STORAGE) != 0) {
                continue;
            }
            if (new ComponentName(ai.packageName, ai.name).equals(cn)) {
                addWidget(cn);
                return true;
            }
        }
        return false;
    }

}
