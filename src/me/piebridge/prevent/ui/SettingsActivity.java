package me.piebridge.prevent.ui;

import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceActivity;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.ui.util.PreventUtils;

/**
 * Created by thom on 15/10/3.
 */
public class SettingsActivity extends PreferenceActivity implements Preference.OnPreferenceChangeListener {

    public static final String KEY_FORCE_STOP_TIMEOUT = "force_stop_timeout";

    @Override
    @SuppressWarnings("deprecation")
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.settings);
        findPreference(KEY_FORCE_STOP_TIMEOUT).setOnPreferenceChangeListener(this);
    }

    @Override
    public boolean onPreferenceChange(Preference preference, Object newValue) {
        String key = preference.getKey();
        if (KEY_FORCE_STOP_TIMEOUT.equals(key)) {
            if (!BuildConfig.RELEASE) {
                try {
                    long timeout = Long.valueOf(String.valueOf(newValue));
                    PreventUtils.updateTimeout(this, timeout);
                } catch (NumberFormatException e) {
                    UILog.d(String.valueOf(newValue) + " is not long", e);
                }
                return true;
            } else {
                return false;
            }
        }
        return true;
    }

}
