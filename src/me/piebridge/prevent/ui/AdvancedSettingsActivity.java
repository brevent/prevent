package me.piebridge.prevent.ui;

import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.view.MenuItem;

import me.piebridge.prevent.R;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.DeprecatedUtils;
import me.piebridge.prevent.ui.util.PreventUtils;
import me.piebridge.prevent.ui.util.ThemeUtils;

/**
 * Created by thom on 15/10/3.
 */
public class AdvancedSettingsActivity extends PreferenceActivity implements Preference.OnPreferenceChangeListener {

    private boolean changed = false;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        ThemeUtils.setTheme(this);
        super.onCreate(savedInstanceState);
        DeprecatedUtils.addPreferencesFromResource(this, R.xml.settings);
        if (getActionBar() != null) {
            getActionBar().setDisplayHomeAsUpEnabled(true);
        }

        for (String key : PreventIntent.KEYS_LONG) {
            setOnPreferenceChangeListener(key);
        }

        for (String key : PreventIntent.KEYS_BOOLEAN) {
            setOnPreferenceChangeListener(key);
        }

        setOnPreferenceChangeListener(PreventIntent.KEY_BACKUP_PREVENT_LIST);
    }

    private void setOnPreferenceChangeListener(String key) {
        Preference preference = DeprecatedUtils.findPreference(this, key);
        if (preference != null) {
            preference.setOnPreferenceChangeListener(this);
        }
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();
        if (id == android.R.id.home) {
            finish();
        }
        return true;
    }

    @Override
    protected void onResume() {
        super.onResume();
        changed = false;
    }


    @Override
    protected void onStop() {
        if (changed) {
            PreventUtils.updateConfiguration(this);
        }
        super.onStop();
    }

    @Override
    public boolean onPreferenceChange(Preference preference, Object newValue) {
        changed = true;
        // tricky to fix for android 2.3
        preference.setShouldDisableView(true);
        return true;
    }

}
