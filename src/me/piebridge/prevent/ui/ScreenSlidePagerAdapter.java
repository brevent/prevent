package me.piebridge.prevent.ui;

import android.app.Fragment;
import android.app.FragmentManager;
import android.support.v13.app.FragmentStatePagerAdapter;
import android.view.ViewGroup;

/**
 * Created by thom on 2016/10/26.
 */
public class ScreenSlidePagerAdapter extends FragmentStatePagerAdapter {

    private final PreventFragment[] mFragments;
    private final String[] mPageTitles;

    public ScreenSlidePagerAdapter(FragmentManager fm, String[] mPageTitles) {
        super(fm);
        this.mPageTitles = mPageTitles;
        mFragments = new PreventFragment[mPageTitles.length];
    }

    @Override
    public Object instantiateItem(ViewGroup container, int position) {
        PreventFragment fragment = (PreventFragment) super.instantiateItem(container, position);
        mFragments[position] = fragment;
        return fragment;
    }

    @Override
    public Fragment getItem(int position) {
        if (position == 0) {
            return new Applications();
        } else if (position == 1) {
            return new PreventList();
        } else {
            return null;
        }
    }

    public PreventFragment getFragment(int position) {
        return mFragments[position];
    }

    @Override
    public int getCount() {
        return mPageTitles.length;
    }

    @Override
    public CharSequence getPageTitle(int position) {
        return mPageTitles[position];
    }

}