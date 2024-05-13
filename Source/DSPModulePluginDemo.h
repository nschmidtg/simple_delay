/*
  ==============================================================================

   This file is part of the JUCE examples.
   Copyright (c) 2022 - Raw Material Software Limited

   The code included in this file is provided under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license. Permission
   To use, copy, modify, and/or distribute this software for any purpose with or
   without fee is hereby granted provided that the above copyright notice and
   this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES,
   WHETHER EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR
   PURPOSE, ARE DISCLAIMED.

  ==============================================================================
*/

/*******************************************************************************
 The block below describes the properties of this PIP. A PIP is a short snippet
 of code that can be read by the Projucer and used to generate a JUCE project.

 BEGIN_JUCE_PIP_METADATA

 name:             DSPModulePluginDemo
 version:          1.0.0
 vendor:           JUCE
 website:          http://juce.com
 description:      An audio plugin using the DSP module.

 dependencies:     juce_audio_basics, juce_audio_devices, juce_audio_formats,
                   juce_audio_plugin_client, juce_audio_processors,
                   juce_audio_utils, juce_core, juce_data_structures, juce_dsp,
                   juce_events, juce_graphics, juce_gui_basics, juce_gui_extra
 exporters:        xcode_mac, vs2022, linux_make

 moduleFlags:      JUCE_STRICT_REFCOUNTEDPOINTER=1

 type:             AudioProcessor
 mainClass:        DspModulePluginDemoAudioProcessor

 useLocalCopy:     1

 END_JUCE_PIP_METADATA

*******************************************************************************/

#pragma once

#include "DemoUtilities.h"

namespace ID
{
   #define PARAMETER_ID(str) constexpr const char* str { #str };

    PARAMETER_ID (inputGain)
    PARAMETER_ID (outputGain)
    PARAMETER_ID (pan)
    PARAMETER_ID (delayEffectValue)
    PARAMETER_ID (delayEffectLowpass)
    PARAMETER_ID (delayEffectHipass)
    PARAMETER_ID (delayEffectFeedback)
    PARAMETER_ID (delayEffectMix)

   #undef PARAMETER_ID
}

template <typename Func, typename... Items>
constexpr void forEach (Func&& func, Items&&... items)
{
    (func (std::forward<Items> (items)), ...);
}

template <typename... Components>
void addAllAndMakeVisible (Component& target, Components&... children)
{
    forEach ([&] (Component& child) { target.addAndMakeVisible (child); }, children...);
}

template <typename... Processors>
void prepareAll (const dsp::ProcessSpec& spec, Processors&... processors)
{
    forEach ([&] (auto& proc) { proc.prepare (spec); }, processors...);
}

template <typename... Processors>
void resetAll (Processors&... processors)
{
    forEach ([] (auto& proc) { proc.reset(); }, processors...);
}

//==============================================================================
class DspModulePluginDemo : public AudioProcessor,
                            private ValueTree::Listener
{
public:
    DspModulePluginDemo()
        : DspModulePluginDemo (AudioProcessorValueTreeState::ParameterLayout{}) {}

    //==============================================================================
    void prepareToPlay (double sampleRate, int samplesPerBlock) final
    {
        const auto channels = jmax (getTotalNumInputChannels(), getTotalNumOutputChannels());

        if (channels == 0)
            return;

        chain.prepare ({ sampleRate, (uint32) samplesPerBlock, (uint32) channels });

        reset();
    }

    void reset() final
    {
        chain.reset();
        update();
    }

    void releaseResources() final {}

    void processBlock (AudioBuffer<float>& buffer, MidiBuffer&) final
    {
        if (jmax (getTotalNumInputChannels(), getTotalNumOutputChannels()) == 0)
            return;

        ScopedNoDenormals noDenormals;

        if (requiresUpdate.load())
            update();

        const auto totalNumInputChannels  = getTotalNumInputChannels();
        const auto totalNumOutputChannels = getTotalNumOutputChannels();

        
        const auto numChannels = jmax (totalNumInputChannels, totalNumOutputChannels);

        auto inoutBlock = dsp::AudioBlock<float> (buffer).getSubsetChannelBlock (0, (size_t) numChannels);
        chain.process (dsp::ProcessContextReplacing<float> (inoutBlock));
    }

    void processBlock (AudioBuffer<double>&, MidiBuffer&) final {}

    //==============================================================================
    AudioProcessorEditor* createEditor() override { return nullptr; }
    bool hasEditor() const override { return false; }

    //==============================================================================
    const String getName() const final { return "DSPModulePluginDemo"; }

    bool acceptsMidi()  const final { return false; }
    bool producesMidi() const final { return false; }
    bool isMidiEffect() const final { return false; }

    double getTailLengthSeconds() const final { return 0.0; }

    //==============================================================================
    int getNumPrograms()    final { return 1; }
    int getCurrentProgram() final { return 0; }
    void setCurrentProgram (int) final {}
    const String getProgramName (int) final { return "None"; }

    void changeProgramName (int, const String&) final {}

    //==============================================================================
    bool isBusesLayoutSupported (const BusesLayout& layout) const final
    {
        return layout == BusesLayout { { AudioChannelSet::stereo() },
                                       { AudioChannelSet::stereo() } };
    }

    //==============================================================================
    void getStateInformation (MemoryBlock& destData) final
    {
        copyXmlToBinary (*apvts.copyState().createXml(), destData);
    }

    void setStateInformation (const void* data, int sizeInBytes) final
    {
        apvts.replaceState (ValueTree::fromXml (*getXmlFromBinary (data, sizeInBytes)));
    }

    int getCurrentIRSize() const { return irSize; }

    using Parameter = AudioProcessorValueTreeState::Parameter;
    using Attributes = AudioProcessorValueTreeStateParameterAttributes;

    // This struct holds references to the raw parameters, so that we don't have to search
    // the APVTS (involving string comparisons and map lookups!) every time a parameter
    // changes.
    struct ParameterReferences
    {
        template <typename Param>
        static void add (AudioProcessorParameterGroup& group, std::unique_ptr<Param> param)
        {
            group.addChild (std::move (param));
        }

        template <typename Param>
        static void add (AudioProcessorValueTreeState::ParameterLayout& group, std::unique_ptr<Param> param)
        {
            group.add (std::move (param));
        }

        template <typename Param, typename Group, typename... Ts>
        static Param& addToLayout (Group& layout, Ts&&... ts)
        {
            auto param = new Param (std::forward<Ts> (ts)...);
            auto& ref = *param;
            add (layout, rawToUniquePtr (param));
            return ref;
        }

        static String valueToTextFunction (float x, int) { return String (x, 2); }
        static float textToValueFunction (const String& str) { return str.getFloatValue(); }

        static auto getBasicAttributes()
        {
            return Attributes().withStringFromValueFunction (valueToTextFunction)
                               .withValueFromStringFunction (textToValueFunction);
        }

        static auto getDbAttributes()           { return getBasicAttributes().withLabel ("dB"); }
        static auto getMsAttributes()           { return getBasicAttributes().withLabel ("ms"); }
        static auto getHzAttributes()           { return getBasicAttributes().withLabel ("Hz"); }
        static auto getPercentageAttributes()   { return getBasicAttributes().withLabel ("%"); }
        static auto getRatioAttributes()        { return getBasicAttributes().withLabel (":1"); }

        static String valueToTextPanFunction (float x, int) { return getPanningTextForValue ((x + 100.0f) / 200.0f); }
        static float textToValuePanFunction (const String& str) { return getPanningValueForText (str) * 200.0f - 100.0f; }

        struct MainGroup
        {
            explicit MainGroup (AudioProcessorParameterGroup& layout)
                : inputGain (addToLayout<Parameter> (layout,
                                                     ParameterID { ID::inputGain, 1 },
                                                     "Input",
                                                     NormalisableRange<float> (-40.0f, 40.0f),
                                                     0.0f,
                                                     getDbAttributes())),
                  outputGain (addToLayout<Parameter> (layout,
                                                      ParameterID { ID::outputGain, 1 },
                                                      "Output",
                                                      NormalisableRange<float> (-40.0f, 40.0f),
                                                      0.0f,
                                                      getDbAttributes())),
                  pan (addToLayout<Parameter> (layout,
                                               ParameterID { ID::pan, 1 },
                                               "Panning",
                                               NormalisableRange<float> (-100.0f, 100.0f),
                                               0.0f,
                                               Attributes().withStringFromValueFunction (valueToTextPanFunction)
                                                           .withValueFromStringFunction (textToValuePanFunction))) {}

            Parameter& inputGain;
            Parameter& outputGain;
            Parameter& pan;
        };

        
        struct DelayEffectGroup
        {
            explicit DelayEffectGroup (AudioProcessorParameterGroup& layout)
                : value (addToLayout<Parameter> (layout,
                                                 ParameterID { ID::delayEffectValue, 1 },
                                                 "Delay",
                                                 NormalisableRange<float> (0.01f, 1000.0f),
                                                 500.0f,
                                                 getMsAttributes())),
                  lowpass (addToLayout<Parameter> (layout,
                                                   ParameterID { ID::delayEffectLowpass, 1 },
                                                   "Low-pass",
                                                   NormalisableRange<float> (20.0f, 22000.0f, 0.0f, 0.25f),
                                                   22000.0f,
                                                   getHzAttributes())),
                  hipass (addToLayout<Parameter> (layout,
                                                   ParameterID { ID::delayEffectHipass, 1 },
                                                   "Hi-pass",
                                                   NormalisableRange<float> (20.0f, 22000.0f, 0.0f, 0.25f),
                                                   20.0f,
                                                   getHzAttributes())),
                  mix (addToLayout<Parameter> (layout,
                                               ParameterID { ID::delayEffectMix, 1 },
                                               "Delay Mix",
                                               NormalisableRange<float> (0.0f, 100.0f),
                                               50.0f,
                                               getPercentageAttributes())),
                  feedback (addToLayout<Parameter> (layout,
                                                    ParameterID { ID::delayEffectFeedback, 1 },
                                                    "Feedback",
                                                    NormalisableRange<float> (-20.0f, 0.0f),
                                                    -5.0f,
                                                    getDbAttributes())) {}

            Parameter& value;
            Parameter& lowpass;
            Parameter& hipass;
            Parameter& mix;
            Parameter& feedback;
        };

        
        explicit ParameterReferences (AudioProcessorValueTreeState::ParameterLayout& layout)
            : main          (addToLayout<AudioProcessorParameterGroup> (layout, "main",          "Main",          "|")),
              delayEffect   (addToLayout<AudioProcessorParameterGroup> (layout, "delayeffect",   "Delay Effect",  "|")) {}

        MainGroup main;
        DelayEffectGroup delayEffect;
    };

    const ParameterReferences& getParameterValues() const noexcept { return parameters; }

    //==============================================================================
    // We store this here so that the editor retains its state if it is closed and reopened
    int indexTab = 0;

private:
    struct LayoutAndReferences
    {
        AudioProcessorValueTreeState::ParameterLayout layout;
        ParameterReferences references;
    };

    explicit DspModulePluginDemo (AudioProcessorValueTreeState::ParameterLayout layout)
        : AudioProcessor (BusesProperties().withInput ("In",   AudioChannelSet::stereo())
                                           .withOutput ("Out", AudioChannelSet::stereo())),
          parameters { layout },
          apvts { *this, nullptr, "state", std::move (layout) }
    {
        apvts.state.addListener (this);

        forEach ([] (dsp::Gain<float>& gain) { gain.setRampDurationSeconds (0.05); },
                 dsp::get<inputGainIndex>  (chain),
                 dsp::get<outputGainIndex> (chain));

        dsp::get<pannerIndex> (chain).setRule (dsp::PannerRule::linear);
    }

    //==============================================================================
    void valueTreePropertyChanged (ValueTree&, const Identifier&) final
    {
        requiresUpdate.store (true);
    }

    //==============================================================================
    void update()
    {
        

        dsp::get<inputGainIndex>  (chain).setGainDecibels (parameters.main.inputGain.get());
        dsp::get<outputGainIndex> (chain).setGainDecibels (parameters.main.outputGain.get());
        dsp::get<pannerIndex> (chain).setPan (parameters.main.pan.get() / 100.0f);

        {
            DelayEffectProcessor& delay = dsp::get<delayEffectIndex> (chain);

            delay.delayEffectValue = (double) parameters.delayEffect.value.get() / 1000.0 * getSampleRate();

            const auto feedbackGain = Decibels::decibelsToGain (parameters.delayEffect.feedback.get(), -100.0f);

            for (auto& volume : delay.delayFeedbackVolume)
                volume.setTargetValue (feedbackGain);

            delay.smoothFilter.setCutoffFrequency (1000.0 / 800.0f);
            delay.lowpass.setCutoffFrequency (parameters.delayEffect.lowpass.get());
            delay.hipass.setCutoffFrequency (parameters.delayEffect.hipass.get());
            delay.mixer.setWetMixProportion (parameters.delayEffect.mix.get() / 100.0f);
        }

        requiresUpdate.store (false);
    }

    //==============================================================================
    static String getPanningTextForValue (float value)
    {
        if (approximatelyEqual (value, 0.5f))
            return "center";

        if (value < 0.5f)
            return String (roundToInt ((0.5f - value) * 200.0f)) + "%L";

        return String (roundToInt ((value - 0.5f) * 200.0f)) + "%R";
    }

    static float getPanningValueForText (String strText)
    {
        if (strText.compareIgnoreCase ("center") == 0 || strText.compareIgnoreCase ("c") == 0)
            return 0.5f;

        strText = strText.trim();

        if (strText.indexOfIgnoreCase ("%L") != -1)
        {
            auto percentage = (float) strText.substring (0, strText.indexOf ("%")).getDoubleValue();
            return (100.0f - percentage) / 100.0f * 0.5f;
        }

        if (strText.indexOfIgnoreCase ("%R") != -1)
        {
            auto percentage = (float) strText.substring (0, strText.indexOf ("%")).getDoubleValue();
            return percentage / 100.0f * 0.5f + 0.5f;
        }

        return 0.5f;
    }

    
    struct DelayEffectProcessor
    {
        DelayEffectProcessor()
        {
            smoothFilter.setType (dsp::FirstOrderTPTFilterType::lowpass);
            lowpass.setType      (dsp::FirstOrderTPTFilterType::lowpass);
            hipass.setType       (dsp::FirstOrderTPTFilterType::highpass);
            mixer.setMixingRule (dsp::DryWetMixingRule::linear);
        }

        void prepare (const dsp::ProcessSpec& spec)
        {
            prepareAll (spec, linear, smoothFilter, lowpass, hipass, mixer);

            for (auto& volume : delayFeedbackVolume)
                volume.reset (spec.sampleRate, 0.05);
        }

        void reset()
        {
            resetAll (linear, smoothFilter, lowpass, hipass, mixer);
            std::fill (lastDelayEffectOutput.begin(), lastDelayEffectOutput.end(), 0.0f);
            std::fill (lastDelaySample.begin(), lastDelaySample.end(), 0.0f);
        }

        template <typename Context>
        void process (Context& context)
        {
            if (context.isBypassed)
                return;

            const auto& inputBlock  = context.getInputBlock();
            const auto& outputBlock = context.getOutputBlock();
            const auto numSamples  = inputBlock.getNumSamples();
            const auto numChannels = inputBlock.getNumChannels();

            mixer.pushDrySamples (inputBlock);
            for (size_t i = 0; i < numSamples; ++i)
            {
                
                
                auto* samplesInL  = inputBlock .getChannelPointer (0);
                auto* samplesInR  = inputBlock .getChannelPointer (1);
                auto* samplesOutL = outputBlock.getChannelPointer (0);
                auto* samplesOutR = outputBlock.getChannelPointer (1);
                
                auto inputL = samplesInR[i] + lastDelayEffectOutput[1];
                auto inputR = samplesInL[i] + lastDelayEffectOutput[0];

                auto delay = smoothFilter.processSample (int (0), delayEffectValue);

                
                const auto outputL = linear.popSample (int (0));
                const auto outputR = linear.popSample (int (1));
                
                linear.pushSample (0, inputL);
                linear.pushSample (1, inputR);
                linear.setDelay ((float) delay);

                auto processedL = lowpass.processSample (int (0), outputL);
                processedL = hipass.processSample(int (0), processedL);
                
                auto processedR = lowpass.processSample (int (1), outputR);
                processedR = hipass.processSample(int (1), processedR);

                samplesOutL[i] = processedL;
                samplesOutR[i] = processedR;

                lastDelayEffectOutput[0] = processedL * delayFeedbackVolume[0].getNextValue();
                lastDelayEffectOutput[1] = processedR * delayFeedbackVolume[1].getNextValue();
            

            }

            mixer.mixWetSamples (outputBlock);
        }

        static constexpr auto effectDelaySamples = 192000;
        dsp::DelayLine<float, dsp::DelayLineInterpolationTypes::Linear>      linear          { effectDelaySamples };


        // Double precision to avoid some approximation issues
        dsp::FirstOrderTPTFilter<double> smoothFilter;

        double delayEffectValue;

        std::array<LinearSmoothedValue<float>, 2> delayFeedbackVolume;
        dsp::FirstOrderTPTFilter<float> lowpass;
        // create a highpass filter
        dsp::FirstOrderTPTFilter<float> hipass;
        //dsp::Panner<float> pingPongPanner;
        dsp::DryWetMixer<float> mixer;
        std::array<float, 2> lastDelayEffectOutput;
        std::array<float, 2> lastDelaySample;
        
        int totalSamples = 0;
    };

    ParameterReferences parameters;
    AudioProcessorValueTreeState apvts;

    using Chain = dsp::ProcessorChain<dsp::Gain<float>,
                                      DelayEffectProcessor,
                                      dsp::Gain<float>,
                                      dsp::Panner<float>>;
    Chain chain;
    

    // We use this enum to index into the chain above
    enum ProcessorIndices
    {
        inputGainIndex,
        delayEffectIndex,
        outputGainIndex,
        pannerIndex
    };

    //==============================================================================
    std::atomic<bool> requiresUpdate { true };
    std::atomic<int> irSize { 0 };

    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (DspModulePluginDemo)
};

//==============================================================================
class DspModulePluginDemoEditor final : public AudioProcessorEditor
{
public:
    explicit DspModulePluginDemoEditor (DspModulePluginDemo& p)
        : AudioProcessorEditor (&p),
          proc (p)
    {
    

        comboEffect.addSectionHeading ("Delay");
        comboEffect.addItem ("Delay line effect", TabDelayLineEffect);

        comboEffect.setSelectedId (proc.indexTab + 1, dontSendNotification);
        comboEffect.onChange = [this]
        {
            proc.indexTab = comboEffect.getSelectedId() - 1;
            updateVisibility();
        };

        addAllAndMakeVisible (*this,
                              comboEffect,
                              labelEffect,
                              basicControls,
                              delayEffectControls);
        labelEffect.setJustificationType (Justification::centredRight);
        labelEffect.attachToComponent (&comboEffect, true);

        updateVisibility();

        setSize (800, 430);
        setResizable (false, false);
    }

    //==============================================================================
    void paint (Graphics& g) override
    {



        background = ImageCache::getFromMemory (BinaryData::background_png, BinaryData::background_pngSize);
        g.drawImageWithin(background, 0, 0, getWidth(), getHeight(), juce::RectanglePlacement::stretchToFit);
    }

    void resized() override
    {
        auto rect = getLocalBounds();
        rect.removeFromTop (topSize);
        rect.removeFromBottom (bottomSize);

        auto rectEffects = rect.removeFromBottom (tabSize);
        auto rectChoice  = rect.removeFromBottom (midSize);

        comboEffect.setBounds (rectChoice.withSizeKeepingCentre (200, 24));

        rect.reduce (80, 0);
        rectEffects.reduce (20, 0);

        basicControls.setBounds (rect);

        forEach ([&] (Component& comp) { comp.setBounds (rectEffects); },
                 delayEffectControls);
    }

private:
    juce::Image background;
    class ComponentWithParamMenu : public Component
    {
    public:
        ComponentWithParamMenu (AudioProcessorEditor& editorIn, RangedAudioParameter& paramIn)
            : editor (editorIn), param (paramIn) {}

        void mouseUp (const MouseEvent& e) override
        {
            if (e.mods.isRightButtonDown())
                if (auto* c = editor.getHostContext())
                    if (auto menuInfo = c->getContextMenuForParameter (&param))
                        menuInfo->getEquivalentPopupMenu().showMenuAsync (PopupMenu::Options{}.withTargetComponent (this)
                                                                                              .withMousePosition());
        }

    private:
        AudioProcessorEditor& editor;
        RangedAudioParameter& param;
    };

    class AttachedSlider final : public ComponentWithParamMenu
    {
    public:
        AttachedSlider (AudioProcessorEditor& editorIn, RangedAudioParameter& paramIn)
            : ComponentWithParamMenu (editorIn, paramIn),
              label ("", paramIn.name),
              attachment (paramIn, slider)
        {
            slider.addMouseListener (this, true);

            addAllAndMakeVisible (*this, slider, label);

            slider.setTextValueSuffix (" " + paramIn.label);

            label.attachToComponent (&slider, false);
            label.setJustificationType (Justification::centred);
        }

        void resized() override { slider.setBounds (getLocalBounds().reduced (0, 40)); }

    private:
        Slider slider { Slider::RotaryVerticalDrag, Slider::TextBoxBelow };
        Label label;
        SliderParameterAttachment attachment;
    };

    class AttachedToggle final : public ComponentWithParamMenu
    {
    public:
        AttachedToggle (AudioProcessorEditor& editorIn, RangedAudioParameter& paramIn)
            : ComponentWithParamMenu (editorIn, paramIn),
              toggle (paramIn.name),
              attachment (paramIn, toggle)
        {
            toggle.addMouseListener (this, true);
            addAndMakeVisible (toggle);
        }

        void resized() override { toggle.setBounds (getLocalBounds()); }

    private:
        ToggleButton toggle;
        ButtonParameterAttachment attachment;
    };

    class AttachedCombo final : public ComponentWithParamMenu
    {
    public:
        AttachedCombo (AudioProcessorEditor& editorIn, RangedAudioParameter& paramIn)
            : ComponentWithParamMenu (editorIn, paramIn),
              combo (paramIn),
              label ("", paramIn.name),
              attachment (paramIn, combo)
        {
            combo.addMouseListener (this, true);

            addAllAndMakeVisible (*this, combo, label);

            label.attachToComponent (&combo, false);
            label.setJustificationType (Justification::centred);
        }

        void resized() override
        {
            combo.setBounds (getLocalBounds().withSizeKeepingCentre (jmin (getWidth(), 150), 24));
        }

    private:
        struct ComboWithItems final : public ComboBox
        {
            explicit ComboWithItems (RangedAudioParameter& param)
            {
                // Adding the list here in the constructor means that the combo
                // is already populated when we construct the attachment below
                addItemList (dynamic_cast<AudioParameterChoice&> (param).choices, 1);
            }
        };

        ComboWithItems combo;
        Label label;
        ComboBoxParameterAttachment attachment;
    };

    //==============================================================================
    void updateVisibility()
    {
        const auto indexEffect = comboEffect.getSelectedId();

        const auto op = [&] (const std::tuple<Component&, int>& tup)
        {
            Component& comp    = std::get<0> (tup);
            const int tabIndex = std::get<1> (tup);
            comp.setVisible (tabIndex == indexEffect);
        };

        forEach (op,
                 std::forward_as_tuple (delayEffectControls, TabDelayLineEffect));
    }

    enum EffectsTabs
    {
        TabDelayLineEffect=1,
    };

    //==============================================================================
    ComboBox comboEffect;
    Label labelEffect { {}, "Audio effect: " };

    struct GetTrackInfo
    {
        // Combo boxes need a lot of room
        Grid::TrackInfo operator() (AttachedCombo&)             const { return 120_px; }

        // Toggles are a bit smaller
        Grid::TrackInfo operator() (AttachedToggle&)            const { return 80_px; }

        // Sliders take up as much room as they can
        Grid::TrackInfo operator() (AttachedSlider&)            const { return 1_fr; }
    };

    template <typename... Components>
    static void performLayout (const Rectangle<int>& bounds, Components&... components)
    {
        Grid grid;
        using Track = Grid::TrackInfo;

        grid.autoColumns     = Track (1_fr);
        grid.autoRows        = Track (1_fr);
        grid.columnGap       = Grid::Px (10);
        grid.rowGap          = Grid::Px (0);
        grid.autoFlow        = Grid::AutoFlow::column;

        grid.templateColumns = { GetTrackInfo{} (components)... };
        grid.items           = { GridItem (components)... };

        grid.performLayout (bounds);
    }

    struct BasicControls final : public Component
    {
        explicit BasicControls (AudioProcessorEditor& editor,
                                const DspModulePluginDemo::ParameterReferences::MainGroup& state)
            : pan       (editor, state.pan),
              input     (editor, state.inputGain),
              output    (editor, state.outputGain)
        {
            addAllAndMakeVisible (*this, pan, input, output);
        }

        void resized() override
        {
            performLayout (getLocalBounds(), input, output, pan);
        }

        AttachedSlider pan, input, output;
    };

    
    struct DelayEffectControls final : public Component
    {
        explicit DelayEffectControls (AudioProcessorEditor& editor,
                                      const DspModulePluginDemo::ParameterReferences::DelayEffectGroup& state)
            : value    (editor, state.value),
              lowpass  (editor, state.lowpass),
              hipass   (editor, state.hipass),
              feedback (editor, state.feedback),
              mix      (editor, state.mix)
        {
            addAllAndMakeVisible (*this, value, lowpass, hipass, feedback, mix);
        }

        void resized() override
        {
            performLayout (getLocalBounds(), value, lowpass, hipass, feedback, mix);
        }

        AttachedSlider value, lowpass, hipass, feedback, mix;
    };

    
    //==============================================================================
    static constexpr auto topSize    = 40,
                          bottomSize = 40,
                          midSize    = 40,
                          tabSize    = 155;

    //==============================================================================
    DspModulePluginDemo& proc;

    BasicControls       basicControls       { *this, proc.getParameterValues().main };
    DelayEffectControls delayEffectControls { *this, proc.getParameterValues().delayEffect };

    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (DspModulePluginDemoEditor)
};

struct DspModulePluginDemoAudioProcessor final : public DspModulePluginDemo
{
    AudioProcessorEditor* createEditor() override
    {
        return new DspModulePluginDemoEditor (*this);
    }

    bool hasEditor() const override { return true; }
};
