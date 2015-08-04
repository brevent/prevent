package me.piebridge.prevent.framework.util;

import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProviderInfo;
import android.content.ComponentName;
import android.content.Context;

import java.util.HashSet;
import java.util.Set;

/**
 * Created by thom on 15/7/31.
 */
public class WidgetUtils {

    private static Set<ComponentName> widgets;

    private WidgetUtils() {

    }

    private static synchronized boolean retrieveWidgets(Context context) {
        if (widgets != null) {
            return false;
        }
        widgets = new HashSet<ComponentName>();
        for (AppWidgetProviderInfo widget : AppWidgetManager.getInstance(context).getInstalledProviders()) {
            widgets.add(widget.provider);
        }
        return true;
    }

    public static boolean isWidget(Context context, ComponentName cn) {
        if (widgets == null) {
            retrieveWidgets(context);
        }
        return widgets.contains(cn);
    }

    public static void cleanWidgets() {
        widgets = null;
    }

}
